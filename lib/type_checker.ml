open Ast

(* Comprehensive type system matching AST types *)
type simple_type =
  | TI8
  | TI16
  | TI32
  | TI64
  | TU8
  | TU16
  | TU32
  | TU64
  | TUsize
  | TF32
  | TF64
  | TBool
  | TChar
  | TStr
  | TVoid
  | TUnit
  | TArray of simple_type * int option
  | TPointer of simple_type
  | TFunction of simple_type list * simple_type
  | TStruct of string * (string * simple_type) list
  | TEnum of string * string list
  | TGeneric of string
  | TNullPtr

(* Type environment to track variable and function types *)
type type_env = {
  variables : (string * simple_type) list;
  functions : (string * (simple_type list * simple_type)) list;
  structs : (string * (string * simple_type) list) list;
  enums : (string * string list) list;
  traits : (string * string list) list;
}

(* Global context for error reporting *)
let current_filename = ref None
let set_filename filename = current_filename := Some filename

let empty_env =
  { variables = []; functions = []; structs = []; enums = []; traits = [] }

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
  try Some (List.assoc name env.variables) with Not_found -> None

let lookup_function env name =
  try Some (List.assoc name env.functions) with Not_found -> None

let lookup_struct env name =
  try Some (List.assoc name env.structs) with Not_found -> None

let lookup_enum env name =
  try Some (List.assoc name env.enums) with Not_found -> None

(* Convert AST literal to simple type *)
let type_of_literal = function
  | IntLit (_, suffix_opt) ->
    (match suffix_opt with
    | Some I8 -> TI8
    | Some I16 -> TI16
    | Some I32 -> TI32
    | Some I64 -> TI64
    | Some U8 -> TU8
    | Some U16 -> TU16
    | Some U32 -> TU32
    | Some U64 -> TU64
    | Some Usize -> TUsize
    | None -> TI32)
  | FloatLit (_, suffix_opt) ->
    (match suffix_opt with
    | Some F32 -> TF32
    | Some F64 -> TF64
    | None -> TF64)
  | StringLit _ -> TStr
  | CharLit _ -> TChar
  | BoolLit _ -> TBool
  | UnitLit -> TUnit
  | NullLit -> TNullPtr

(* Convert AST types to simple types *)
let rec ast_type_to_simple_type = function
  | PrimType prim ->
    (match prim with
    | I8 -> TI8
    | I16 -> TI16
    | I32 -> TI32
    | I64 -> TI64
    | U8 -> TU8
    | U16 -> TU16
    | U32 -> TU32
    | U64 -> TU64
    | Usize -> TUsize
    | F32 -> TF32
    | F64 -> TF64
    | Bool -> TBool
    | Char -> TChar
    | Str -> TStr
    | Void -> TVoid)
  | ArrayType (elem_type, _size_expr) ->
    let elem_simple = ast_type_to_simple_type elem_type in
    TArray (elem_simple, None)
  | PointerType inner_type ->
    let inner_simple = ast_type_to_simple_type inner_type in
    TPointer inner_simple
  | FunctionType (param_types, return_type_opt) ->
    let param_simples = List.map ast_type_to_simple_type param_types in
    let return_simple =
      match return_type_opt with
      | Some ret_type -> ast_type_to_simple_type ret_type
      | None -> TUnit
    in
    TFunction (param_simples, return_simple)
  | PathType (path, _generics_opt) ->
    let name = String.concat "::" path in
    TGeneric name
  | GenericType name -> TGeneric name

(* Type compatibility helpers *)
let normalize_type = function
  | TI8 | TI16 | TI32 | TI64 | TU8 | TU16 | TU32 | TU64 | TUsize -> TI32
  | TF32 | TF64 -> TF64
  | t -> t

(* Built-in environment *)
let builtin_env =
  let env = empty_env in
  let env = add_function env "print" [ TStr ] TUnit in
  let env = add_function env "println" [ TStr ] TUnit in
  let env = add_function env "int_to_string" [ TI32 ] TStr in
  let env = add_function env "string_to_int" [ TStr ] TI32 in
  env

(* Enhanced type_error_at function *)
let type_error_at pos message =
  raise
    (Error.type_error ~filename:None ~line:pos.line ~column:pos.column ~offset:0
       message)

(* Global context for error reporting *)
let current_filename = ref None
let set_filename filename = current_filename := Some filename

let empty_env =
  { variables = []; functions = []; structs = []; enums = []; traits = [] }

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
  try Some (List.assoc name env.variables) with Not_found -> None

let lookup_function env name =
  try Some (List.assoc name env.functions) with Not_found -> None

let lookup_struct env name =
  try Some (List.assoc name env.structs) with Not_found -> None

let lookup_enum env name =
  try Some (List.assoc name env.enums) with Not_found -> None

(* Convert AST literal to simple type *)
let type_of_literal = function
  | IntLit (_, suffix_opt) ->
    (match suffix_opt with
    | Some I8 -> TI8
    | Some I16 -> TI16
    | Some I32 -> TI32
    | Some I64 -> TI64
    | Some U8 -> TU8
    | Some U16 -> TU16
    | Some U32 -> TU32
    | Some U64 -> TU64
    | Some Usize -> TUsize
    | None -> TI32)
  | FloatLit (_, suffix_opt) ->
    (match suffix_opt with
    | Some F32 -> TF32
    | Some F64 -> TF64
    | None -> TF64)
  | StringLit _ -> TStr
  | CharLit _ -> TChar
  | BoolLit _ -> TBool
  | UnitLit -> TUnit
  | NullLit -> TNullPtr

(* Convert AST types to simple types *)
let rec ast_type_to_simple_type = function
  | PrimType prim ->
    (match prim with
    | I8 -> TI8
    | I16 -> TI16
    | I32 -> TI32
    | I64 -> TI64
    | U8 -> TU8
    | U16 -> TU16
    | U32 -> TU32
    | U64 -> TU64
    | Usize -> TUsize
    | F32 -> TF32
    | F64 -> TF64
    | Bool -> TBool
    | Char -> TChar
    | Str -> TStr
    | Void -> TVoid)
  | ArrayType (elem_type, _size_expr) ->
    let elem_simple = ast_type_to_simple_type elem_type in
    TArray (elem_simple, None)
  | PointerType inner_type ->
    let inner_simple = ast_type_to_simple_type inner_type in
    TPointer inner_simple
  | FunctionType (param_types, return_type_opt) ->
    let param_simples = List.map ast_type_to_simple_type param_types in
    let return_simple =
      match return_type_opt with
      | Some ret_type -> ast_type_to_simple_type ret_type
      | None -> TUnit
    in
    TFunction (param_simples, return_simple)
  | PathType (path, _generics_opt) ->
    let name = String.concat "::" path in
    TGeneric name
  | GenericType name -> TGeneric name

(* Type compatibility helpers *)
let normalize_type = function
  | TI8 | TI16 | TI32 | TI64 | TU8 | TU16 | TU32 | TU64 | TUsize -> TI32
  | TF32 | TF64 -> TF64
  | t -> t

(* Built-in environment *)
let builtin_env =
  let env = empty_env in
  let env = add_function env "print" [ TStr ] TUnit in
  let env = add_function env "println" [ TStr ] TUnit in
  let env = add_function env "int_to_string" [ TI32 ] TStr in
  let env = add_function env "string_to_int" [ TStr ] TI32 in
  env

(* Enhanced error reporting with position *)
let type_error_at pos msg =
  raise
    (Error.type_error ~filename:!current_filename ~line:pos.line
       ~column:pos.column ~offset:0 msg)

(* Type checking for expressions *)
let rec check_expression env = function
  | Literal (lit, _) -> type_of_literal lit
  | Identifier (name, pos) ->
    (match lookup_variable env name with
    | Some typ -> typ
    | None -> type_error_at pos ("Undefined variable: " ^ name))
  | BinaryOp (left, op, right, pos) ->
    let left_type = check_expression env left in
    let right_type = check_expression env right in
    let left_norm = normalize_type left_type in
    let right_norm = normalize_type right_type in
    (match (op, left_norm, right_norm) with
    | (Add | Sub | Mul | Div | Mod), TI32, TI32 -> left_type
    | (Add | Sub | Mul | Div), TF64, TF64 -> left_type
    | (Eq | Ne | Lt | Le | Gt | Ge), TI32, TI32 -> TBool
    | (Eq | Ne | Lt | Le | Gt | Ge), TF64, TF64 -> TBool
    | (Eq | Ne), TStr, TStr -> TBool
    | (Eq | Ne), TBool, TBool -> TBool
    | (And | Or), TBool, TBool -> TBool
    | _ -> type_error_at pos "Type mismatch in binary operation")
  | UnaryOp (op, expr, pos) ->
    let expr_type = check_expression env expr in
    let expr_norm = normalize_type expr_type in
    (match (op, expr_norm) with
    | Not, TBool -> TBool
    | Neg, TI32 -> expr_type
    | Neg, TF64 -> expr_type
    | Ref, _ -> TPointer expr_type
    | Deref, TPointer inner -> inner
    | Deref, _ -> type_error_at pos "Cannot dereference non-pointer type"
    | Sizeof, _ -> TI32
    | _ -> type_error_at pos "Type mismatch in unary operation")
  | FunctionCall (func_expr, args, pos) ->
    let param_types, return_type =
      match func_expr with
      | Identifier (name, func_pos) ->
        (match lookup_function env name with
        | Some (params, ret) -> (params, ret)
        | None -> type_error_at func_pos ("Undefined function: " ^ name))
      | PathExpr (path, func_pos) ->
        let func_name = String.concat "::" path in
        (match lookup_function env func_name with
        | Some (params, ret) -> (params, ret)
        | None -> type_error_at func_pos ("Function not found: " ^ func_name))
      | _ ->
        let func_type = check_expression env func_expr in
        (match func_type with
        | TFunction (params, ret) -> (params, ret)
        | _ -> type_error_at pos "Expression is not callable")
    in
    let arg_types = List.map (check_expression env) args in
    if List.length param_types = List.length arg_types then (
      List.iter2
        (fun expected actual ->
          if normalize_type actual <> normalize_type expected then
            type_error_at pos "Function argument type mismatch")
        param_types arg_types ;
      return_type)
    else
      type_error_at pos "Function called with wrong number of arguments"
  | Block ((statements, expr_opt), _) ->
    let env' = List.fold_left check_statement env statements in
    (match expr_opt with
    | Some expr -> check_expression env' expr
    | None -> TUnit)
  | If (cond, then_block, else_block_opt, pos) ->
    let cond_type = check_expression env cond in
    if cond_type <> TBool then
      type_error_at pos "If condition must be boolean" ;
    let then_type = check_block env then_block in
    (match else_block_opt with
    | Some else_block ->
      let else_type = check_block env else_block in
      if then_type = else_type then
        then_type
      else
        type_error_at pos "If branches have different types"
    | None -> TUnit)
  | Return (expr_opt, _) ->
    (match expr_opt with
    | Some expr -> check_expression env expr
    | None -> TUnit)
  | ArrayExpr (exprs, pos) ->
    (match exprs with
    | [] -> TArray (TUnit, Some 0)
    | first_expr :: rest_exprs ->
      let first_type = check_expression env first_expr in
      List.iter
        (fun expr ->
          let expr_type = check_expression env expr in
          if normalize_type expr_type <> normalize_type first_type then
            type_error_at pos "Array elements must have same type")
        rest_exprs ;
      TArray (first_type, Some (List.length exprs)))
  | Index (array_expr, index_expr, pos) ->
    let array_type = check_expression env array_expr in
    let index_type = check_expression env index_expr in
    if normalize_type index_type <> TI32 then
      type_error_at pos "Array index must be integer" ;
    (match array_type with
    | TArray (elem_type, _) -> elem_type
    | _ -> type_error_at pos "Index operation requires array type")
  | FieldAccess (struct_expr, field_name, pos) ->
    let struct_type = check_expression env struct_expr in
    (match struct_type with
    | TStruct (struct_name, fields) ->
      (try List.assoc field_name fields
       with Not_found ->
         (match lookup_struct env struct_name with
         | Some global_fields ->
           (try List.assoc field_name global_fields
            with Not_found ->
              type_error_at pos ("Struct field not found: " ^ field_name))
         | None -> type_error_at pos ("Struct field not found: " ^ field_name)))
    | _ -> type_error_at pos "Field access requires struct type")
  | StructExpr (path, field_exprs, _) ->
    let field_types =
      List.map
        (fun (field_name, expr) ->
          let field_type = check_expression env expr in
          (field_name, field_type))
        field_exprs
    in
    let struct_name = String.concat "::" path in
    TStruct (struct_name, field_types)
  | Cast (expr, target_type, pos) ->
    let source_type = check_expression env expr in
    let target_simple = ast_type_to_simple_type target_type in
    let source_norm = normalize_type source_type in
    let target_norm = normalize_type target_simple in
    let is_valid_cast =
      match (source_norm, target_norm) with
      | TI32, TI32 -> true
      | TI32, TF64 -> true
      | TF64, TI32 -> true
      | TI32, TStr -> true
      | TStr, TI32 -> true
      | TBool, TI32 -> true
      | TI32, TBool -> true
      | TPointer _, TI32 -> true
      | TI32, TPointer _ -> true
      | _ when source_norm = target_norm -> true
      | _ -> false
    in
    if is_valid_cast then
      target_simple
    else
      type_error_at pos "Invalid cast between types"
  | PathExpr (path, _pos) ->
    let name = String.concat "::" path in
    let enum_check =
      List.fold_left
        (fun acc (enum_name, variants) ->
          match acc with
          | Some t -> Some t
          | None ->
            if List.mem (List.hd (List.rev path)) variants then
              Some (TEnum (enum_name, variants))
            else
              None)
        None env.enums
    in
    (match enum_check with
    | Some enum_type -> enum_type
    | None ->
      (match lookup_function env name with
      | Some (params, ret) -> TFunction (params, ret)
      | None ->
        (match lookup_variable env name with
        | Some var_type -> var_type
        | None -> TGeneric name)))
  | PointerAccess (ptr_expr, _field, pos) ->
    let ptr_type = check_expression env ptr_expr in
    (match ptr_type with
    | TPointer inner_type -> inner_type
    | _ -> type_error_at pos "Pointer access requires pointer type")
  | Match (scrutinee, match_arms, _) ->
    ignore (check_expression env scrutinee) ;
    let arm_types =
      List.map
        (fun (MatchArm (_pattern, expr)) -> check_expression env expr)
        match_arms
    in
    (match arm_types with
    | [] -> TUnit
    | first_type :: rest_types ->
      List.iter
        (fun arm_type ->
          if normalize_type arm_type <> normalize_type first_type then
            let pos =
              get_expression_position
                (match match_arms with
                | MatchArm (_, expr) :: _ -> expr
                | [] -> scrutinee)
            in
            type_error_at pos "Match arms have different types")
        rest_types ;
      first_type)
  | While (cond, body, pos) ->
    let cond_type = check_expression env cond in
    if cond_type <> TBool then
      type_error_at pos "While condition must be boolean" ;
    ignore (check_block env body) ;
    TUnit
  | Loop (body, _) ->
    ignore (check_block env body) ;
    TUnit
  | For (var, iter_expr, body, _pos) ->
    let iter_type = check_expression env iter_expr in
    let iter_norm = normalize_type iter_type in
    let var_type = (match iter_norm with
      | TArray (elem_type, _) -> elem_type
      | _ -> type_error_at (get_expression_position iter_expr) "For loop iterator must be an array type"
    ) in
    let new_env = add_variable env var var_type in
    ignore (check_block new_env body) ;
    TUnit
  | Break (_expr_opt, _) -> TUnit
  | Continue _ -> TUnit
  | Range (start, end_expr, _pos) ->
    let start_type = check_expression env start in
    let end_type = check_expression env end_expr in
    (* Range expressions should have integer types *)
    let start_norm = normalize_type start_type in
    let end_norm = normalize_type end_type in
    (match (start_norm, end_norm) with
    | (TI32, TI32) -> TArray (TI32, None)  (* Return array type for ranges *)
    | (TI64, TI64) -> TArray (TI64, None)
    | (TUsize, TUsize) -> TArray (TUsize, None)
    | _ -> type_error_at (get_expression_position start) "Range expressions require integer types")

(* Type checking for blocks *)
and check_block env (statements, expr_opt) =
  let final_env = List.fold_left check_statement env statements in
  match expr_opt with
  | Some expr -> check_expression final_env expr
  | None -> TUnit

(* Type checking for statements *)
and check_statement env = function
  | ExprStmt expr ->
    ignore (check_expression env expr) ;
    env
  | ItemStmt _item ->
    (* For now, just ignore item statements in function bodies *)
    env
  | LetStmt (_, name, typ_opt, expr_opt) ->
    (match expr_opt with
    | Some expr ->
      let expr_type = check_expression env expr in
      (* Validate against declared type if present *)
      (match typ_opt with
      | Some declared_type ->
        let expected_type = ast_type_to_simple_type declared_type in
        if normalize_type expr_type <> normalize_type expected_type then
          type_error_at (get_expression_position expr) "Variable type mismatch" ;
        add_variable env name expected_type
      | None -> add_variable env name expr_type)
    | None ->
      (* Variable declaration without initialization *)
      (match typ_opt with
      | Some declared_type ->
        let var_type = ast_type_to_simple_type declared_type in
        add_variable env name var_type
      | None ->
        raise
          (Error.parse_error ~filename:None ~line:1 ~column:1 ~offset:0
             "Variable declaration requires either type annotation or \
              initializer")))
  | AssignStmt (lvalue, assign_op, expr) ->
    (* Type check the RHS expression *)
    let rhs_type = check_expression env expr in

    (* Type check the lvalue and get its type *)
    let lvalue_type =
      match lvalue with
      | LvalueId name ->
        (match lookup_variable env name with
        | Some var_type -> var_type
        | None ->
          raise
            (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
               ("Undefined variable in assignment: " ^ name)))
      | LvalueDeref lval ->
        (* For pointer dereferencing, recursively check the lvalue *)
        let ptr_type =
          match lval with
          | LvalueId name ->
            (match lookup_variable env name with
            | Some (TPointer inner_type) -> inner_type
            | Some _ ->
              raise
                (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                   "Cannot dereference non-pointer type")
            | None ->
              raise
                (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                   ("Undefined variable in dereference: " ^ name)))
          | _ ->
            raise
              (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                 "Complex lvalue dereferencing not fully supported")
        in
        ptr_type
      | LvalueIndex (lval, index_expr) ->
        (* For array indexing, check the base lvalue and index *)
        let base_type =
          match lval with
          | LvalueId name ->
            (match lookup_variable env name with
            | Some (TArray (elem_type, _)) -> elem_type
            | Some _ ->
              raise
                (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                   "Cannot index non-array type")
            | None ->
              raise
                (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                   ("Undefined variable in index: " ^ name)))
          | _ ->
            raise
              (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                 "Complex lvalue indexing not fully supported")
        in
        let index_type = check_expression env index_expr in
        if normalize_type index_type <> TI32 then
          type_error_at
            (get_expression_position index_expr)
            "Array index must be integer" ;
        base_type
      | LvalueField (lval, field_name) ->
        (* For field access, check the base lvalue type *)
        let base_type =
          match lval with
          | LvalueId name ->
            (match lookup_variable env name with
            | Some struct_type -> struct_type
            | None ->
              raise
                (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                   ("Undefined variable in field access: " ^ name)))
          | _ ->
            raise
              (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                 "Complex lvalue field access not fully supported")
        in
        (match base_type with
        | TStruct (struct_name, fields) ->
          (try List.assoc field_name fields
           with Not_found ->
             (match lookup_struct env struct_name with
             | Some global_fields ->
               (try List.assoc field_name global_fields
                with Not_found ->
                  raise
                    (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                       ("Struct field not found: " ^ field_name)))
             | None ->
               raise
                 (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                    ("Struct field not found: " ^ field_name))))
        | _ ->
          raise
            (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
               "Field access requires struct type"))
      | LvaluePointer (lval, field_name) ->
        (* For pointer field access, check the base lvalue type *)
        let base_type =
          match lval with
          | LvalueId name ->
            (match lookup_variable env name with
            | Some (TPointer inner_type) -> inner_type
            | Some _ ->
              raise
                (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                   "Pointer access requires pointer type")
            | None ->
              raise
                (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                   ("Undefined variable in pointer access: " ^ name)))
          | _ ->
            raise
              (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                 "Complex lvalue pointer access not fully supported")
        in
        (match base_type with
        | TStruct (struct_name, fields) ->
          (try List.assoc field_name fields
           with Not_found ->
             (match lookup_struct env struct_name with
             | Some global_fields ->
               (try List.assoc field_name global_fields
                with Not_found ->
                  raise
                    (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                       ("Struct field not found: " ^ field_name)))
             | None ->
               raise
                 (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
                    ("Struct field not found: " ^ field_name))))
        | _ ->
          raise
            (Error.type_error ~filename:None ~line:1 ~column:1 ~offset:0
               "Pointer field access requires struct type"))
    in

    (* Validate assignment operation *)
    let result_type =
      match assign_op with
      | Assign ->
        if normalize_type lvalue_type <> normalize_type rhs_type then
          type_error_at
            (get_expression_position expr)
            "Assignment type mismatch" ;
        lvalue_type
      | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign ->
        let lvalue_norm = normalize_type lvalue_type in
        let rhs_norm = normalize_type rhs_type in
        if lvalue_norm <> rhs_norm then
          type_error_at
            (get_expression_position expr)
            "Compound assignment type mismatch" ;
        (match lvalue_norm with
        | TI32 | TF64 -> lvalue_type
        | _ ->
          type_error_at
            (get_expression_position expr)
            "Compound assignment requires numeric types")
      | BitAndAssign | BitOrAssign | BitXorAssign | ShlAssign | ShrAssign ->
        let lvalue_norm = normalize_type lvalue_type in
        let rhs_norm = normalize_type rhs_type in
        if lvalue_norm <> rhs_norm then
          type_error_at
            (get_expression_position expr)
            "Bitwise assignment type mismatch" ;
        (match lvalue_norm with
        | TI32 -> lvalue_type
        | _ ->
          type_error_at
            (get_expression_position expr)
            "Bitwise assignment requires integer types")
    in
    ignore result_type ;
    env

(* Type checking for top-level items *)
and check_item env = function
  | Function func_def ->
    (* Add function to environment first *)
    let param_types =
      List.map
        (fun param ->
          (* Convert AST types to simple types *)
          ast_type_to_simple_type param.param_type)
        func_def.func_params
    in
    let return_type =
      match func_def.func_return with
      | Some ast_type -> ast_type_to_simple_type ast_type
      | None -> TUnit
    in
    (* Default to unit for functions without return type *)
    let env_with_func =
      add_function env func_def.func_name param_types return_type
    in

    (* Create local environment with parameters *)
    let local_env =
      List.fold_left
        (fun acc param ->
          let param_type = ast_type_to_simple_type param.param_type in
          add_variable acc param.param_name param_type)
        env_with_func func_def.func_params
    in

    (* Type check function body *)
    let statements, _expr_opt = func_def.func_body in
    ignore (List.fold_left check_statement local_env statements) ;
    env_with_func
  | Struct struct_def ->
    (* Add struct to environment *)
    let field_types =
      List.map
        (fun field_def ->
          let field_type = ast_type_to_simple_type field_def.field_type in
          (field_def.field_name, field_type))
        struct_def.struct_fields
    in
    add_struct env struct_def.struct_name field_types
  | Enum enum_def ->
    (* Add enum to environment *)
    let variant_names =
      List.map (fun variant -> variant.variant_name) enum_def.enum_variants
    in
    add_enum env enum_def.enum_name variant_names
  | Trait trait_def ->
    (* Add trait to environment *)
    let method_names =
      List.map
        (fun trait_item ->
          match trait_item with
          | TraitFunction (name, _generics, _params, _return) -> name
          | AssociatedType (name, _) -> name)
        trait_def.trait_items
    in
    add_trait env trait_def.trait_name method_names
  | Impl impl_def ->
    (* Add impl methods to environment *)
    List.fold_left
      (fun acc_env impl_item ->
        match impl_item with
        | ImplFunction func_def ->
          (* Add method with qualified name *)
          let method_name =
            match impl_def.impl_trait with
            | Some trait_path ->
              let trait_name = String.concat "::" trait_path in
              trait_name ^ "::" ^ func_def.func_name
            | None ->
              (* Instance method - need type name *)
              let type_name =
                match impl_def.impl_type with
                | PathType (path, _) -> String.concat "::" path
                | _ -> "UnknownType"
              in
              type_name ^ "::" ^ func_def.func_name
          in
          let param_types =
            List.map
              (fun param -> ast_type_to_simple_type param.param_type)
              func_def.func_params
          in
          let return_type =
            match func_def.func_return with
            | Some ast_type -> ast_type_to_simple_type ast_type
            | None -> TUnit
          in
          add_function acc_env method_name param_types return_type
        | ImplTypeAlias (_name, _type) ->
          (* Type aliases within impl blocks *)
          acc_env)
      env impl_def.impl_items
  | GlobalVar (_, _, _, name, typ_opt, expr) ->
    let expr_type = check_expression env expr in
    (* Validate against declared type if present *)
    (match typ_opt with
    | Some declared_type ->
      let expected_type = ast_type_to_simple_type declared_type in
      if normalize_type expr_type <> normalize_type expected_type then
        type_error_at
          (get_expression_position expr)
          "Global variable type mismatch" ;
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
    ignore (List.fold_left check_item builtin_env program) ;
    (* If successful, print success message *)
    Printf.printf "Type checking completed successfully for %s\n" filename
  with
  | Error.CompilerError error ->
    (* Re-raise the original error *)
    raise (Error.CompilerError error)
  | Failure msg ->
    let error =
      Error.type_error ~filename:(Some filename) ~line:1 ~column:1 ~offset:0 msg
    in
    raise error
