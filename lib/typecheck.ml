open Ast
open Token

exception TypeError of string * span option

type ty_env = (string * ty) list
type func_env = (string * func) list

let lookup_var env name =
    match List.assoc_opt name env with
        | Some t -> t
        | None ->
            raise
              (TypeError ("Unbound variable: " ^ name, None))

let lookup_func funcs name =
    match List.assoc_opt name funcs with
        | Some f -> f
        | None ->
            raise
              (TypeError ("Unknown function: " ^ name, None))

let rec type_expr (env : ty_env) (funcs : func_env)
    (e : expr) : ty =
    match e with
        | Int _ -> TyInt
        | Float _ -> TyFloat
        | Bool _ -> TyBool
        | Char _ -> TyChar
        | Unit -> TyUnit
        | Ident name -> lookup_var env name
        | Binary (lhs, op, rhs) -> (
            let lt = type_expr env funcs lhs in
            let rt = type_expr env funcs rhs in
                match op with
                    | Add | Sub | Mul | Div ->
                        if lt = TyInt && rt = TyInt then
                          TyInt
                        else if lt = TyFloat && rt = TyFloat
                        then
                          TyFloat
                        else
                          raise
                            (TypeError
                               ( "Invalid operands for \
                                  arithmetic",
                                 None ))
                    | Eq | Neq | Lt | Le | Gt | Ge ->
                        if
                          lt = rt
                          && (lt = TyInt || lt = TyFloat
                            || lt = TyChar || lt = TyBool)
                        then
                          TyBool
                        else
                          raise
                            (TypeError
                               ( "Invalid operands for \
                                  comparison",
                                 None ))
                    | And | Or ->
                        if lt = TyBool && rt = TyBool then
                          TyBool
                        else
                          raise
                            (TypeError
                               ( "Invalid operands for \
                                  logical operation",
                                 None )))
        | Call (Ident fname, args) ->
            let { params; return_ty; _ } =
                lookup_func funcs fname
            in
                if List.length params <> List.length args
                then
                  raise
                    (TypeError
                       ( "Function arity mismatch: " ^ fname,
                         None ));
                List.iter2
                  (fun (param_name, param_ty) arg_expr ->
                    let arg_ty =
                        type_expr env funcs arg_expr
                    in
                        if arg_ty <> param_ty then
                          raise
                            (TypeError
                               ( Printf.sprintf
                                   "Type mismatch for \
                                    argument '%s'"
                                   param_name,
                                 None )))
                  params args;
                return_ty
        | Call _ ->
            raise
              (TypeError
                 ( "Only named functions can be called for \
                    now",
                   None ))

let rec type_stmt (env : ty_env) (funcs : func_env)
    (stmt : stmt) : ty_env * ty option =
    match stmt with
        | Let (name, ty_opt, expr) ->
            let expr_ty = type_expr env funcs expr in
                (match ty_opt with
                    | Some expected_ty ->
                        if expr_ty <> expected_ty then
                          raise
                            (TypeError
                               ( "Let binding type \
                                  mismatch for " ^ name,
                                 None ))
                    | None -> ());
                ((name, expr_ty) :: env, None)
        | Assign (name, expr) ->
            let expected = lookup_var env name in
            let actual = type_expr env funcs expr in
                if expected <> actual then
                  raise
                    (TypeError
                       ( "Assignment type mismatch to \
                          variable " ^ name,
                         None ));
                (env, None)
        | Return expr ->
            let t = type_expr env funcs expr in
                (env, Some t)
        | Expr e ->
            let _ = type_expr env funcs e in
                (env, None)
        | ExprValue e ->
            let t = type_expr env funcs e in
                (env, Some t)
        | If (cond, then_block, else_block_opt) ->
            let cond_ty = type_expr env funcs cond in
                if cond_ty <> TyBool then
                  raise
                    (TypeError
                       ("Condition of if must be bool", None));
                let _ =
                    type_block env funcs then_block
                in
                let _ =
                    match else_block_opt with
                        | Some b -> type_block env funcs b
                        | None -> (env, None)
                in
                    (* Ignore the type for now *)
                    (env, None)

and type_block (env : ty_env) (funcs : func_env)
    (block : block) : ty_env * ty option =
    let rec go env = function
        | [] -> (env, None)
        | [ stmt ] -> type_stmt env funcs stmt
        | stmt :: rest ->
            let env', _ = type_stmt env funcs stmt in
                go env' rest
    in
        go env block

let type_func funcs env (f : func) : unit =
    let param_env =
        List.map (fun (name, ty) -> (name, ty)) f.params
    in
    let full_env = env @ param_env in
    let _, ret = type_block full_env funcs f.body in
        match ret with
            | Some ty when ty <> f.return_ty ->
                raise
                  (TypeError
                     ( Printf.sprintf
                         "Function '%s' returns %s but \
                          declared %s"
                         f.name (string_of_ty ty)
                         (string_of_ty f.return_ty),
                       None ))
            | _ -> ()

let type_program (prog : program) : unit =
    (* Separate function definitions *)
    let funcs =
        List.fold_left
          (fun acc decl ->
            match decl with
                | Func f -> (f.name, f) :: acc
                | GlobalLet _ -> acc)
          [] prog
    in

    (* First build global environment from global lets *)
    let globals =
        List.fold_left
          (fun env decl ->
            match decl with
                | GlobalLet (name, ty_opt, expr) ->
                    let expr_ty =
                        type_expr env funcs expr
                    in
                        (match ty_opt with
                            | Some expected_ty ->
                                if expected_ty <> expr_ty
                                then
                                  raise
                                    (TypeError
                                       ( Printf.sprintf
                                           "Global \
                                            variable '%s' \
                                            type mismatch \
                                            (got %s, \
                                            expected %s)"
                                           name
                                           (string_of_ty
                                              expr_ty)
                                           (string_of_ty
                                              expected_ty),
                                         None ))
                            | None -> ());
                        (name, expr_ty) :: env
                | Func _ -> env)
          [] prog
    in

    (* Type-check all functions with global variables available *)
    List.iter
      (function
        | Func f -> type_func funcs (globals @ f.params) f
        | GlobalLet _ -> ())
      prog
