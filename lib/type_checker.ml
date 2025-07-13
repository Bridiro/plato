open Ast

(* Comprehensive type system matching AST types *)
type simple_type =
  | TI8 | TI16 | TI32 | TI64
  | TU8 | TU16 | TU32 | TU64 | TUsize
  | TF32 | TF64
  | TBool | TChar | TStr | TVoid | TUnit
  | TArray of simple_type * int option  (* element type, optional size *)
  | TPointer of simple_type
  | TFunction of simple_type list * simple_type
  | TStruct of string * (string * simple_type) list  (* name, fields *)
  | TEnum of string * string list  (* name, variants *)
  | TGeneric of string
  | TNullPtr

(* Type environment to track variable and function types *)
type type_env = {
  variables: (string * simple_type) list;
  functions: (string * (simple_type list * simple_type)) list;
  structs: (string * (string * simple_type) list) list;  (* struct_name -> fields *)
  enums: (string * string list) list;  (* enum_name -> variants *)
  traits: (string * string list) list;  (* trait_name -> methods *)
}

(* Type error exception with position info *)
exception TypeCheckError of string * int * int (* message, line, column *)

(* Helper to get current position from lexer *)
let get_current_position () =
  let (line, column, _pos) = Lexer.get_last_token_position () in
  (line, column)

let empty_env = { variables = []; functions = []; structs = []; enums = []; traits = [] }

let add_variable env name typ =
  { env with variables = (name, typ) :: env.variables }

let add_function env name param_types return_type =
  { env with functions = (name, (param_types, return_type)) :: env.functions }

let add_struct env name fields =
  { env with structs = (name, fields) :: env.structs }

let add_enum env name variants =
  { env with enums = (name, variants) :: env.enums }

let add_trait env name methods =
  { env with traits = (name, methods) :: env.traits }

let lookup_variable env name =
  try Some (List.assoc name env.variables)
  with Not_found -> None

let lookup_function env name =
  try Some (List.assoc name env.functions)
  with Not_found -> None

let lookup_struct env name =
  try Some (List.assoc name env.structs)
  with Not_found -> None

let lookup_enum env name =
  try Some (List.assoc name env.enums)
  with Not_found -> None

(* Convert AST literal to simple type with backwards compatibility *)
let type_of_literal = function
  | IntLit (_, suffix_opt) -> 
      (match suffix_opt with
       | Some I8 -> TI8  | Some I16 -> TI16  | Some I32 -> TI32  | Some I64 -> TI64
       | Some U8 -> TU8  | Some U16 -> TU16  | Some U32 -> TU32  | Some U64 -> TU64
       | Some Usize -> TUsize | None -> TI32)
  | FloatLit (_, suffix_opt) ->
      (match suffix_opt with
       | Some F32 -> TF32 | Some F64 -> TF64 | None -> TF64)
  | StringLit _ -> TStr
  | CharLit _ -> TChar
  | BoolLit _ -> TBool
  | UnitLit -> TUnit
  | NullLit -> TNullPtr

(* Convert AST types to simple types *)
let rec ast_type_to_simple_type = function
  | PrimType prim -> 
      (match prim with
       | I8 -> TI8 | I16 -> TI16 | I32 -> TI32 | I64 -> TI64
       | U8 -> TU8 | U16 -> TU16 | U32 -> TU32 | U64 -> TU64 | Usize -> TUsize
       | F32 -> TF32 | F64 -> TF64
       | Bool -> TBool | Char -> TChar | Str -> TStr | Void -> TVoid)
  | ArrayType (elem_type, _size_expr) ->
      let elem_simple = ast_type_to_simple_type elem_type in
      TArray (elem_simple, None)
  | PointerType inner_type ->
      let inner_simple = ast_type_to_simple_type inner_type in
      TPointer inner_simple
  | FunctionType (param_types, return_type_opt) ->
      let param_simples = List.map ast_type_to_simple_type param_types in
      let return_simple = match return_type_opt with
        | Some ret_type -> ast_type_to_simple_type ret_type
        | None -> TUnit in
      TFunction (param_simples, return_simple)
  | PathType (path, _generics_opt) ->
      let name = String.concat "::" path in
      TGeneric name
  | GenericType name -> TGeneric name

(* Type compatibility helpers for backwards compatibility *)
let normalize_type = function
  | TI8 | TI16 | TI32 | TI64 | TU8 | TU16 | TU32 | TU64 | TUsize -> TI32  (* Treat as int for compatibility *)
  | TF32 | TF64 -> TF64  (* Treat as float for compatibility *)
  | TStr -> TStr
  | t -> t

(* Built-in functions with normalized types *)
let builtin_env = 
  let env = empty_env in
  let env = add_function env "print" [TStr] TUnit in
  let env = add_function env "println" [TStr] TUnit in
  let env = add_function env "int_to_string" [TI32] TStr in
  let env = add_function env "string_to_int" [TStr] TI32 in
  env

(* Type checking for expressions *)
let rec check_expression env = function
  | Literal lit -> type_of_literal lit
  | Identifier name ->
      (match lookup_variable env name with
       | Some typ -> typ
       | None -> 
           let (line, column) = get_current_position () in
           raise (TypeCheckError ("Undefined variable: " ^ name, line, column)))
  | BinaryOp (left, op, right) ->
      let left_type = check_expression env left in
      let right_type = check_expression env right in
      let left_norm = normalize_type left_type in
      let right_norm = normalize_type right_type in
      (match op, left_norm, right_norm with
       | (Add | Sub | Mul | Div | Mod), TI32, TI32 -> left_type  (* Return original type, not normalized *)
       | (Add | Sub | Mul | Div), TF64, TF64 -> left_type
       | (Eq | Ne | Lt | Le | Gt | Ge), TI32, TI32 -> TBool
       | (Eq | Ne | Lt | Le | Gt | Ge), TF64, TF64 -> TBool
       | (Eq | Ne), TStr, TStr -> TBool
       | (Eq | Ne), TBool, TBool -> TBool
       | (And | Or), TBool, TBool -> TBool
       | _ -> 
           let (line, column) = get_current_position () in
           raise (TypeCheckError ("Type mismatch in binary operation", line, column)))
  | UnaryOp (op, expr) ->
      let expr_type = check_expression env expr in
      let expr_norm = normalize_type expr_type in
      (match op, expr_norm with
       | Not, TBool -> TBool
       | Neg, TI32 -> expr_type  (* Return original type *)
       | Neg, TF64 -> expr_type
       | _ -> 
           let (line, column) = get_current_position () in
           raise (TypeCheckError ("Type mismatch in unary operation", line, column)))
  | FunctionCall (func_expr, args) ->
      (* Handle different types of function expressions *)
      let (func_name, param_types, return_type) = match func_expr with
        | Identifier name ->
            (match lookup_function env name with
             | Some (params, ret) -> (name, params, ret)
             | None -> 
                 let (line, column) = get_current_position () in
                 raise (TypeCheckError ("Undefined function: " ^ name, line, column)))
        | FieldAccess (struct_expr, method_name) ->
            (* Method call - check struct type and look up method *)
            let struct_type = check_expression env struct_expr in
            (match struct_type with
             | TStruct (struct_name, _) ->
                 let method_full_name = struct_name ^ "::" ^ method_name in
                 (match lookup_function env method_full_name with
                  | Some (params, ret) -> (method_full_name, params, ret)
                  | None ->
                      let (line, column) = get_current_position () in
                      raise (TypeCheckError ("Method not found: " ^ method_name, line, column)))
             | _ ->
                 let (line, column) = get_current_position () in
                 raise (TypeCheckError ("Method call on non-struct type", line, column)))
        | PathExpr path ->
            (* Static function call or constructor *)
            let func_name = String.concat "::" path in
            (match lookup_function env func_name with
             | Some (params, ret) -> (func_name, params, ret)
             | None ->
                 let (line, column) = get_current_position () in
                 raise (TypeCheckError ("Function not found: " ^ func_name, line, column)))
        | _ ->
            (* For other complex expressions, assume they return a function type *)
            let func_type = check_expression env func_expr in
            (match func_type with
             | TFunction (params, ret) -> ("closure", params, ret)
             | _ ->
                 let (line, column) = get_current_position () in
                 raise (TypeCheckError ("Expression is not callable", line, column)))
      in
      
      (* Type check arguments *)
      let arg_types = List.map (check_expression env) args in
      if List.length param_types = List.length arg_types then (
        List.iter2 (fun expected actual ->
          if normalize_type actual <> normalize_type expected then
            let (line, column) = get_current_position () in
            raise (TypeCheckError ("Function " ^ func_name ^ " expects different argument types", line, column))
        ) param_types arg_types;
        return_type
      ) else
        let (line, column) = get_current_position () in
        raise (TypeCheckError ("Function " ^ func_name ^ " called with wrong number of arguments", line, column))
  | Block (statements, expr_opt) ->
      let env' = List.fold_left check_statement env statements in
      (match expr_opt with
       | Some expr -> check_expression env' expr
       | None -> TUnit)
  | If (cond, then_block, else_block_opt) ->
      let cond_type = check_expression env cond in
      if cond_type <> TBool then (
        let (line, column) = get_current_position () in
        raise (TypeCheckError ("If condition must be boolean", line, column))
      );
      let then_type = check_expression env (Block then_block) in
      (match else_block_opt with
       | Some else_block ->
           let else_type = check_expression env (Block else_block) in
           if then_type = else_type then then_type
           else (
             let (line, column) = get_current_position () in
             raise (TypeCheckError ("If branches have different types", line, column))
           )
       | None -> TUnit)
  | Return expr_opt ->
      (match expr_opt with
       | Some expr -> check_expression env expr
       | None -> TUnit)
  | ArrayExpr exprs ->
      (* Check array elements and ensure they have the same type *)
      (match exprs with
       | [] -> TArray (TUnit, Some 0)  (* Empty array *)
       | first_expr :: rest_exprs ->
           let first_type = check_expression env first_expr in
           List.iter (fun expr -> 
             let expr_type = check_expression env expr in
             if normalize_type expr_type <> normalize_type first_type then (
               let (line, column) = get_current_position () in
               raise (TypeCheckError ("Array elements must have same type", line, column))
             )
           ) rest_exprs;
           TArray (first_type, Some (List.length exprs)))
  | Index (array_expr, index_expr) ->
      let array_type = check_expression env array_expr in
      let index_type = check_expression env index_expr in
      let index_norm = normalize_type index_type in
      if index_norm <> TI32 then (
        let (line, column) = get_current_position () in
        raise (TypeCheckError ("Array index must be integer", line, column))
      );
      (* Extract element type from array *)
      (match array_type with
       | TArray (elem_type, _) -> elem_type
       | _ -> 
           let (line, column) = get_current_position () in
           raise (TypeCheckError ("Index operation requires array type", line, column)))
  | FieldAccess (struct_expr, field_name) ->
      let struct_type = check_expression env struct_expr in
      (match struct_type with
       | TStruct (struct_name, fields) ->
           (* First try to find field in the struct type itself *)
           (try List.assoc field_name fields
            with Not_found -> 
              (* Then try to find it in the global struct definition *)
              match lookup_struct env struct_name with
              | Some global_fields ->
                  (try List.assoc field_name global_fields
                   with Not_found ->
                     let (line, column) = get_current_position () in
                     raise (TypeCheckError ("Struct field not found: " ^ field_name, line, column)))
              | None ->
                  let (line, column) = get_current_position () in
                  raise (TypeCheckError ("Struct field not found: " ^ field_name, line, column)))
       | _ -> 
           let (line, column) = get_current_position () in
           raise (TypeCheckError ("Field access requires struct type", line, column)))
  | StructExpr (path, field_exprs) ->
      (* Check that all field expressions are valid *)
      let field_types = List.map (fun (field_name, expr) -> 
        let field_type = check_expression env expr in
        (field_name, field_type)
      ) field_exprs in
      (* Return a struct type with the given fields *)
      let struct_name = String.concat "::" path in
      TStruct (struct_name, field_types)
  | Cast (expr, target_type) ->
      let source_type = check_expression env expr in
      let target_simple = ast_type_to_simple_type target_type in
      let source_norm = normalize_type source_type in
      let target_norm = normalize_type target_simple in
      
      (* Validate that the cast is reasonable *)
      let is_valid_cast = match source_norm, target_norm with
        | TI32, TI32 -> true  (* Same type (normalized) *)
        | TI32, TF64 -> true  (* int to float *)
        | TF64, TI32 -> true  (* float to int *)
        | TI32, TStr -> true  (* int to string *)
        | TStr, TI32 -> true  (* string to int *)
        | TBool, TI32 -> true (* bool to int *)
        | TI32, TBool -> true (* int to bool *)
        | TPointer _, TI32 -> true (* pointer to int *)
        | TI32, TPointer _ -> true (* int to pointer *)
        | _ when source_norm = target_norm -> true  (* Same normalized type *)
        | _ -> false
      in
      
      if is_valid_cast then target_simple
      else (
        let (line, column) = get_current_position () in
        raise (TypeCheckError ("Invalid cast from source type to target type", line, column))
      )
  | PathExpr path ->
      let name = String.concat "::" path in
      (* Check if it's an enum variant *)
      let enum_check = List.fold_left (fun acc (enum_name, variants) ->
        match acc with
        | Some t -> Some t
        | None -> 
            if List.mem (List.hd (List.rev path)) variants then
              Some (TEnum (enum_name, variants))
            else None
      ) None env.enums in
      
      (match enum_check with
       | Some enum_type -> enum_type
       | None ->
           (* Check if it's a function or variable *)
           (match lookup_function env name with
            | Some (params, ret) -> TFunction (params, ret)
            | None ->
                (match lookup_variable env name with
                 | Some var_type -> var_type
                 | None -> 
                     (* Treat as generic type/constant *)
                     TGeneric name)))
  | PointerAccess (ptr_expr, _field) ->
      (* Check pointer expression *)
      let ptr_type = check_expression env ptr_expr in
      (match ptr_type with
       | TPointer inner_type -> inner_type
       | _ -> 
           let (line, column) = get_current_position () in
           raise (TypeCheckError ("Pointer access requires pointer type", line, column)))
  | Match (scrutinee, match_arms) ->
      ignore (check_expression env scrutinee);
      (* Check all match arms and ensure they return the same type *)
      let arm_types = List.map (fun (MatchArm (_pattern, expr)) ->
        check_expression env expr
      ) match_arms in
      (match arm_types with
       | [] -> TUnit
       | first_type :: rest_types ->
           List.iter (fun arm_type ->
             if normalize_type arm_type <> normalize_type first_type then (
               let (line, column) = get_current_position () in
               raise (TypeCheckError ("Match arms have different types", line, column))
             )
           ) rest_types;
           first_type)
  | While (cond, body) ->
      let cond_type = check_expression env cond in
      if cond_type <> TBool then (
        let (line, column) = get_current_position () in
        raise (TypeCheckError ("While condition must be boolean", line, column))
      );
      ignore (check_expression env (Block body));
      TUnit
  | Loop body ->
      ignore (check_expression env (Block body));
      TUnit
  | For (_var, iter_expr, body) ->
      (* Check iterator expression *)
      ignore (check_expression env iter_expr);
      ignore (check_expression env (Block body));
      TUnit
  | Break _expr_opt ->
      TUnit (* Break statements don't return values in this context *)
  | Continue ->
      TUnit

(* Type checking for statements *)
and check_statement env = function
  | ExprStmt expr -> 
      ignore (check_expression env expr);
      env
  | LetStmt (_, name, typ_opt, expr_opt) ->
      (match expr_opt with
       | Some expr ->
           let expr_type = check_expression env expr in
           (* Validate against declared type if present *)
           (match typ_opt with
            | Some declared_type ->
                let expected_type = ast_type_to_simple_type declared_type in
                if normalize_type expr_type <> normalize_type expected_type then (
                  let (line, column) = get_current_position () in
                  raise (TypeCheckError ("Variable type mismatch", line, column))
                );
                add_variable env name expected_type
            | None -> add_variable env name expr_type)
       | None -> 
           (* Variable declaration without initialization *)
           (match typ_opt with
            | Some declared_type ->
                let var_type = ast_type_to_simple_type declared_type in
                add_variable env name var_type
            | None ->
                let (line, column) = get_current_position () in
                raise (TypeCheckError ("Variable declaration requires either type annotation or initializer", line, column))))
  | AssignStmt (lvalue, assign_op, expr) ->
      (* Type check the RHS expression *)
      let rhs_type = check_expression env expr in
      
      (* Type check the lvalue and get its type *)
      let lvalue_type = match lvalue with
        | LvalueId name ->
            (match lookup_variable env name with
             | Some var_type -> var_type
             | None ->
                 let (line, column) = get_current_position () in
                 raise (TypeCheckError ("Undefined variable in assignment: " ^ name, line, column)))
        | LvalueDeref lval ->
            (* For pointer dereferencing, recursively check the lvalue *)
            let ptr_type = match lval with
              | LvalueId name ->
                  (match lookup_variable env name with
                   | Some (TPointer inner_type) -> inner_type
                   | Some _ ->
                       let (line, column) = get_current_position () in
                       raise (TypeCheckError ("Cannot dereference non-pointer type", line, column))
                   | None ->
                       let (line, column) = get_current_position () in
                       raise (TypeCheckError ("Undefined variable in dereference: " ^ name, line, column)))
              | _ ->
                  let (line, column) = get_current_position () in
                  raise (TypeCheckError ("Complex lvalue dereferencing not fully supported", line, column))
            in ptr_type
        | LvalueIndex (lval, index_expr) ->
            (* For array indexing, check the base lvalue and index *)
            let base_type = match lval with
              | LvalueId name ->
                  (match lookup_variable env name with
                   | Some (TArray (elem_type, _)) -> elem_type
                   | Some _ ->
                       let (line, column) = get_current_position () in
                       raise (TypeCheckError ("Cannot index non-array type", line, column))
                   | None ->
                       let (line, column) = get_current_position () in
                       raise (TypeCheckError ("Undefined variable in index: " ^ name, line, column)))
              | _ ->
                  let (line, column) = get_current_position () in
                  raise (TypeCheckError ("Complex lvalue indexing not fully supported", line, column))
            in
            let index_type = check_expression env index_expr in
            if normalize_type index_type <> TI32 then (
              let (line, column) = get_current_position () in
              raise (TypeCheckError ("Array index must be integer", line, column))
            );
            base_type
        | LvalueField (lval, field_name) ->
            (* For field access, check the base lvalue type *)
            let base_type = match lval with
              | LvalueId name ->
                  (match lookup_variable env name with
                   | Some struct_type -> struct_type
                   | None ->
                       let (line, column) = get_current_position () in
                       raise (TypeCheckError ("Undefined variable in field access: " ^ name, line, column)))
              | _ ->
                  let (line, column) = get_current_position () in
                  raise (TypeCheckError ("Complex lvalue field access not fully supported", line, column))
            in
            (match base_type with
             | TStruct (struct_name, fields) ->
                 (try List.assoc field_name fields
                  with Not_found ->
                    match lookup_struct env struct_name with
                    | Some global_fields ->
                        (try List.assoc field_name global_fields
                         with Not_found ->
                           let (line, column) = get_current_position () in
                           raise (TypeCheckError ("Struct field not found: " ^ field_name, line, column)))
                    | None ->
                        let (line, column) = get_current_position () in
                        raise (TypeCheckError ("Struct field not found: " ^ field_name, line, column)))
             | _ ->
                 let (line, column) = get_current_position () in
                 raise (TypeCheckError ("Field access on non-struct type", line, column)))
        | LvaluePointer (lval, field_name) ->
            (* For pointer field access, check the base pointer type *)
            let base_type = match lval with
              | LvalueId name ->
                  (match lookup_variable env name with
                   | Some (TPointer struct_type) -> struct_type
                   | Some _ ->
                       let (line, column) = get_current_position () in
                       raise (TypeCheckError ("Pointer field access on non-pointer type", line, column))
                   | None ->
                       let (line, column) = get_current_position () in
                       raise (TypeCheckError ("Undefined variable in pointer access: " ^ name, line, column)))
              | _ ->
                  let (line, column) = get_current_position () in
                  raise (TypeCheckError ("Complex lvalue pointer access not fully supported", line, column))
            in
            (match base_type with
             | TStruct (struct_name, fields) ->
                 (try List.assoc field_name fields
                  with Not_found ->
                    match lookup_struct env struct_name with
                    | Some global_fields ->
                        (try List.assoc field_name global_fields
                         with Not_found ->
                           let (line, column) = get_current_position () in
                           raise (TypeCheckError ("Struct field not found: " ^ field_name, line, column)))
                    | None ->
                        let (line, column) = get_current_position () in
                        raise (TypeCheckError ("Struct field not found: " ^ field_name, line, column)))
             | _ ->
                 let (line, column) = get_current_position () in
                 raise (TypeCheckError ("Pointer field access on non-struct type", line, column)))
      in
      
      (* Check assignment compatibility based on operation *)
      let expected_type = match assign_op with
        | Assign -> lvalue_type
        | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign ->
            if normalize_type lvalue_type <> TI32 && normalize_type lvalue_type <> TF64 then (
              let (line, column) = get_current_position () in
              raise (TypeCheckError ("Arithmetic assignment requires numeric lvalue", line, column))
            );
            lvalue_type
        | BitAndAssign | BitOrAssign | BitXorAssign | ShlAssign | ShrAssign ->
            if normalize_type lvalue_type <> TI32 then (
              let (line, column) = get_current_position () in
              raise (TypeCheckError ("Bitwise assignment requires integer lvalue", line, column))
            );
            lvalue_type
      in
      
      if normalize_type rhs_type <> normalize_type expected_type then (
        let (line, column) = get_current_position () in
        raise (TypeCheckError ("Assignment type mismatch", line, column))
      );
      
      env
  | ItemStmt item ->
      (* Handle nested items by delegating to check_item *)
      check_item env item

(* Type checking for top-level items *)
and check_item env = function
  | Function func_def ->
      (* Add function to environment first *)
      let param_types = List.map (fun param -> 
        (* Convert AST types to simple types *)
        ast_type_to_simple_type param.param_type
      ) func_def.func_params in
      let return_type = match func_def.func_return with
        | Some ast_type -> ast_type_to_simple_type ast_type
        | None -> TUnit in  (* Default to unit for functions without return type *)
      let env_with_func = add_function env func_def.func_name param_types return_type in
      
      (* Create local environment with parameters *)
      let local_env = List.fold_left (fun acc param ->
        let param_type = ast_type_to_simple_type param.param_type in
        add_variable acc param.param_name param_type
      ) env_with_func func_def.func_params in
      
      (* Type check function body *)
      let (statements, _expr_opt) = func_def.func_body in
      ignore (List.fold_left check_statement local_env statements);
      env_with_func
      
  | Struct struct_def ->
      (* Add struct to environment *)
      let field_types = List.map (fun field_def ->
        let field_type = ast_type_to_simple_type field_def.field_type in
        (field_def.field_name, field_type)
      ) struct_def.struct_fields in
      add_struct env struct_def.struct_name field_types
      
  | Enum enum_def ->
      (* Add enum to environment *)
      let variant_names = List.map (fun variant ->
        variant.variant_name
      ) enum_def.enum_variants in
      add_enum env enum_def.enum_name variant_names
      
  | Trait trait_def ->
      (* Add trait to environment *)
      let method_names = List.map (fun trait_item ->
        match trait_item with
        | TraitFunction (name, _generics, _params, _return) -> name
        | AssociatedType (name, _) -> name
      ) trait_def.trait_items in
      add_trait env trait_def.trait_name method_names
      
  | Impl impl_def ->
      (* Add impl methods to environment *)
      List.fold_left (fun acc_env impl_item ->
        match impl_item with
        | ImplFunction func_def ->
            (* Add method with qualified name *)
            let method_name = match impl_def.impl_trait with
              | Some trait_path ->
                  let trait_name = String.concat "::" trait_path in
                  trait_name ^ "::" ^ func_def.func_name
              | None ->
                  (* Instance method - need type name *)
                  let type_name = match impl_def.impl_type with
                    | PathType (path, _) -> String.concat "::" path
                    | _ -> "UnknownType"
                  in
                  type_name ^ "::" ^ func_def.func_name
            in
            let param_types = List.map (fun param ->
              ast_type_to_simple_type param.param_type
            ) func_def.func_params in
            let return_type = match func_def.func_return with
              | Some ast_type -> ast_type_to_simple_type ast_type
              | None -> TUnit in
            add_function acc_env method_name param_types return_type
        | ImplTypeAlias (_name, _type) ->
            (* Type aliases within impl blocks *)
            acc_env
      ) env impl_def.impl_items
      
  | GlobalVar (_, _, _, name, typ_opt, expr) ->
      let expr_type = check_expression env expr in
      (* Validate against declared type if present *)
      (match typ_opt with
       | Some declared_type ->
           let expected_type = ast_type_to_simple_type declared_type in
           if normalize_type expr_type <> normalize_type expected_type then (
             let (line, column) = get_current_position () in
             raise (TypeCheckError ("Global variable type mismatch", line, column))
           );
           add_variable env name expected_type
       | None -> add_variable env name expr_type)
       
  | TypeAlias (_vis, _name, _generics, _target_type) ->
      (* Type aliases - for now just pass through *)
      env
      
  | Use (_vis, _path) ->
      (* Use statements - for now just pass through *)
      env
      
  | Mod (_vis, _name, items_opt) ->
      (* Module definitions *)
      (match items_opt with
       | Some items ->
           (* Type check all items in the module *)
           List.fold_left check_item env items
       | None -> env)

(* Main type checking function *)
let type_check_program program filename =
  try
    ignore (List.fold_left check_item builtin_env program);
  with
  | TypeCheckError (msg, line, column) ->
      (* Create a proper error with position information *)
      let error = Error.type_error ~filename:(Some filename) ~line ~column ~offset:0 msg in
      raise error
  | Failure msg ->
      let error = Error.type_error ~filename:(Some filename) ~line:1 ~column:1 ~offset:0 msg in
      raise error
