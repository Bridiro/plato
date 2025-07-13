open Ast

(* Simplified type system for basic type checking *)
type simple_type =
  | TInt
  | TFloat
  | TString
  | TBool
  | TUnit
  | TChar

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

(* Convert AST literal to simple type *)
let type_of_literal = function
  | IntLit (_, _) -> TInt
  | FloatLit (_, _) -> TFloat
  | StringLit _ -> TString
  | CharLit _ -> TChar
  | BoolLit _ -> TBool
  | UnitLit -> TUnit
  | NullLit -> TUnit (* For now, treat null as unit *)

(* Built-in functions *)
let builtin_env = 
  let env = empty_env in
  let env = add_function env "print" [TString] TUnit in
  let env = add_function env "println" [TString] TUnit in
  let env = add_function env "int_to_string" [TInt] TString in
  let env = add_function env "string_to_int" [TString] TInt in
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
      (match op, left_type, right_type with
       | (Add | Sub | Mul | Div | Mod), TInt, TInt -> TInt
       | (Add | Sub | Mul | Div), TFloat, TFloat -> TFloat
       | (Eq | Ne | Lt | Le | Gt | Ge), TInt, TInt -> TBool
       | (Eq | Ne | Lt | Le | Gt | Ge), TFloat, TFloat -> TBool
       | (Eq | Ne), TString, TString -> TBool
       | (Eq | Ne), TBool, TBool -> TBool
       | (And | Or), TBool, TBool -> TBool
       | _ -> 
           let (line, column) = get_current_position () in
           raise (TypeCheckError ("Type mismatch in binary operation", line, column)))
  | UnaryOp (op, expr) ->
      let expr_type = check_expression env expr in
      (match op, expr_type with
       | Not, TBool -> TBool
       | Neg, TInt -> TInt
       | Neg, TFloat -> TFloat
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
  | _ -> 
      let (line, column) = get_current_position () in
      raise (TypeCheckError ("Expression type checking not implemented for this construct", line, column))

(* Type checking for statements *)
and check_statement env = function
  | ExprStmt expr -> 
      ignore (check_expression env expr);
      env
  | LetStmt (_, name, _typ_opt, expr_opt) ->
      (match expr_opt with
       | Some expr ->
           let expr_type = check_expression env expr in
           (* For now, ignore declared type annotation *)
           add_variable env name expr_type
       | None -> 
           (* Variable declaration without initialization - for now just add with TUnit *)
           add_variable env name TUnit)
  | _ -> 
      let (line, column) = get_current_position () in
      raise (TypeCheckError ("Statement type checking not implemented for this construct", line, column))

(* Type checking for top-level items *)
let check_item env = function
  | Function func_def ->
      (* Add function to environment first *)
      let param_types = List.map (fun _param -> 
        (* Convert AST types to simple types - for now just assume TInt for everything *)
        TInt
      ) func_def.func_params in
      let return_type = TInt in (* For now, assume int return type *)
      let env_with_func = add_function env func_def.func_name param_types return_type in
      
      (* Create local environment with parameters *)
      let local_env = List.fold_left (fun acc param ->
        add_variable acc param.param_name TInt (* For now, assume all params are int *)
      ) env_with_func func_def.func_params in
      
      (* Type check function body *)
      let (statements, _expr_opt) = func_def.func_body in
      ignore (List.fold_left check_statement local_env statements);
      env_with_func
  | GlobalVar (_, _, _, name, _typ_opt, expr) ->
      let expr_type = check_expression env expr in
      add_variable env name expr_type
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
