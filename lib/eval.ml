open Ast

(* Runtime values *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VChar of char
  | VUnit

let string_of_value = function
    | VInt i -> string_of_int i
    | VFloat f -> string_of_float f
    | VBool b -> string_of_bool b
    | VChar c -> Printf.sprintf "'%c'" c
    | VUnit -> "()"

(* Environments for variables *)
module Env = Map.Make (String)

type env = value Env.t

(* Top-level function environment *)
type func_env = (string * func) list

(* Binary operations *)
let eval_binop op v1 v2 =
    match (op, v1, v2) with
        | Add, VInt a, VInt b -> VInt (a + b)
        | Sub, VInt a, VInt b -> VInt (a - b)
        | Mul, VInt a, VInt b -> VInt (a * b)
        | Div, VInt a, VInt b -> VInt (a / b)
        | Eq, VInt a, VInt b -> VBool (a = b)
        | Neq, VInt a, VInt b -> VBool (a <> b)
        | Lt, VInt a, VInt b -> VBool (a < b)
        | Le, VInt a, VInt b -> VBool (a <= b)
        | Gt, VInt a, VInt b -> VBool (a > b)
        | Ge, VInt a, VInt b -> VBool (a >= b)
        | And, VBool a, VBool b -> VBool (a && b)
        | Or, VBool a, VBool b -> VBool (a || b)
        | _ ->
            failwith "Invalid operands for binary operation"

(* Expression evaluation *)
let rec eval_expr (funcs : func_env) (env : env) (e : expr)
    : value =
    match e with
        | Int i -> VInt i
        | Float f -> VFloat f
        | Bool b -> VBool b
        | Char c -> VChar c
        | Unit -> VUnit
        | Ident name -> (
            try Env.find name env
            with Not_found ->
              failwith ("Unbound variable: " ^ name))
        | Binary (lhs, op, rhs) ->
            let lval = eval_expr funcs env lhs in
            let rval = eval_expr funcs env rhs in
                eval_binop op lval rval
        | Call (Ident fname, args) ->
            let arg_values =
                List.map (eval_expr funcs env) args
            in
                eval_funcall funcs env fname arg_values
        | Call (callee, _) ->
            failwith
              ("Cannot call non-identifier expression: "
              ^ string_of_expr callee)

(* Statement execution *)
and exec_stmt (funcs : func_env) (env : env) (stmt : stmt) :
    env * value option =
    match stmt with
        | Let (name, e) ->
            let v = eval_expr funcs env e in
                (Env.add name v env, None)
        | Assign (name, e) ->
            let v = eval_expr funcs env e in
                if Env.mem name env then
                  (Env.add name v env, None)
                else
                  failwith
                    ("Assignment to undeclared variable: "
                   ^ name)
        | Expr e ->
            let _ = eval_expr funcs env e in
                (env, None)
        | Return e ->
            let v = eval_expr funcs env e in
                (env, Some v)
        | If (cond, then_blk, else_blk_opt) -> (
            let c = eval_expr funcs env cond in
                match c with
                    | VBool true ->
                        exec_block funcs env then_blk
                    | VBool false -> (
                        match else_blk_opt with
                            | Some blk ->
                                exec_block funcs env blk
                            | None -> (env, None))
                    | _ ->
                        failwith
                          "Condition in if must be a \
                           boolean")

(* Block = list of statements, returns last value if Return is hit *)
and exec_block (funcs : func_env) (env : env)
    (stmts : block) : env * value option =
    match stmts with
        | [] -> (env, None)
        | stmt :: rest -> (
            let env', result = exec_stmt funcs env stmt in
                match result with
                    | Some _ ->
                        (env', result)
                        (* propagate return *)
                    | None -> exec_block funcs env' rest)

and eval_funcall (funcs : func_env) (env : env)
    (name : string) (args : value list) : value =
    match List.assoc_opt name funcs with
        | Some { name = _; params; body; _ } -> (
            if List.length params <> List.length args then
              failwith
                ("Incorrect number of arguments for \
                  function: " ^ name);
            let local_env =
                List.fold_left2
                  (fun acc (param_name, _) arg ->
                    Env.add param_name arg acc)
                  env params args
            in
            let _, result =
                exec_block funcs local_env body
            in
                match result with
                    | Some v -> v
                    | None -> VUnit)
        | None -> failwith ("Undefined function: " ^ name)

(* Entry point for running a Plato program *)
let run_program (prog : program) : value =
    let globals =
        List.fold_left
          (fun acc decl ->
            match decl with
                | GlobalLet (name, e) ->
                    let v = eval_expr [] acc e in
                        Env.add name v acc
                | _ -> acc)
          Env.empty prog
    in
    let funcs =
        List.filter_map
          (function
            | Func f -> Some (f.name, f)
            | _ -> None)
          prog
    in
        eval_funcall funcs globals "main" []
