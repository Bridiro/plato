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
}

(* Type error exception with position info *)
exception TypeCheckError of string * int * int (* message, line, column *)

(* Helper to get current position from lexer *)
let get_current_position () =
  let (line, column, _pos) = Lexer.get_last_token_position () in
  (line, column)

let empty_env = { variables = []; functions = [] }

let add_variable env name typ =
  { env with variables = (name, typ) :: env.variables }

let add_function env name param_types return_type =
  { env with functions = (name, (param_types, return_type)) :: env.functions }

let lookup_variable env name =
  try Some (List.assoc name env.variables)
  with Not_found -> None

let lookup_function env name =
  try Some (List.assoc name env.functions)
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
      (match func_expr with
       | Identifier name ->
           (match lookup_function env name with
            | Some (param_types, return_type) ->
                let arg_types = List.map (check_expression env) args in
                if List.length param_types = List.length arg_types then (
                  List.iter2 (fun expected actual ->
                    if expected <> actual then
                      let (line, column) = get_current_position () in
                      raise (TypeCheckError ("Function " ^ name ^ " expects different argument types", line, column))
                  ) param_types arg_types;
                  return_type
                ) else
                  let (line, column) = get_current_position () in
                  raise (TypeCheckError ("Function " ^ name ^ " called with wrong number of arguments", line, column))
            | None -> 
                let (line, column) = get_current_position () in
                raise (TypeCheckError ("Undefined function: " ^ name, line, column)))
       | _ -> 
           let (line, column) = get_current_position () in
           raise (TypeCheckError ("Complex function expressions not supported yet", line, column)))
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
      (* For now, return a generic type - proper struct field lookup would require struct definitions *)
      (match struct_type with
       | TStruct (_name, fields) ->
           (try List.assoc field_name fields
            with Not_found -> 
              let (line, column) = get_current_position () in
              raise (TypeCheckError ("Struct field not found: " ^ field_name, line, column)))
       | _ -> 
           (* For now, assume field access on unknown types returns i32 *)
           TI32)
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
      (* Check source expression and validate cast *)
      let source_type = check_expression env expr in
      let target_simple = ast_type_to_simple_type target_type in
      (* For now, allow all casts - proper cast validation would check type compatibility *)
      ignore source_type;
      target_simple
  | PathExpr path ->
      (* For now, treat path expressions as enum/constant references *)
      let name = String.concat "::" path in
      TGeneric name
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
  | AssignStmt (_lvalue, _assign_op, expr) ->
      (* Type check assignment - for now just check that RHS is valid *)
      ignore (check_expression env expr);
      (* TODO: Proper lvalue type checking *)
      env
  | ItemStmt _item ->
      (* TODO: Handle nested items *)
      env

(* Type checking for top-level items *)
let check_item env = function
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
  | _ -> 
      (* For other constructs, just return the environment unchanged *)
      env

(* Main type checking function *)
let type_check_program program filename =
  try
    ignore (List.fold_left check_item builtin_env program);
    Printf.printf "✓ Type checking passed\n"
  with
  | TypeCheckError (msg, line, column) ->
      (* Create a proper error with position information *)
      let error = Error.type_error ~filename:(Some filename) ~line ~column ~offset:0 msg in
      raise error
  | Failure msg ->
      let error = Error.type_error ~filename:(Some filename) ~line:1 ~column:1 ~offset:0 msg in
      raise error
