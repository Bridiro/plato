open Token
open Ast

(* === Parser State === *)
type parser_state = {
  tokens : located_token array;
  mutable pos : int;
}

let init_parser tokens =
    { tokens = Array.of_list tokens; pos = 0 }

let peek state =
    if state.pos < Array.length state.tokens then
      Some state.tokens.(state.pos)
    else
      None

let advance state =
    if state.pos < Array.length state.tokens then
      state.pos <- state.pos + 1

let expect state expected =
    match peek state with
        | Some { node; span = _ } when node = expected ->
            advance state
        | Some { node; span } ->
            failwith
              (Printf.sprintf "Expected %s but got %s at %s"
                 (show_token expected)
                 (show_token node) (show_span span))
        | None -> failwith "Unexpected end of input"

let eat_ident state =
    match peek state with
        | Some { node = Ident name; _ } ->
            advance state;
            name
        | Some { node; span } ->
            failwith
              (Printf.sprintf
                 "Expected identifier but got %s at %s"
                 (show_token node) (show_span span))
        | None -> failwith "Expected identifier but got EOF"

let parse_ty state =
    match peek state with
        | Some { node = Ident "int"; _ } ->
            advance state;
            TyInt
        | Some { node = Ident "float"; _ } ->
            advance state;
            TyFloat
        | Some { node = Ident "bool"; _ } ->
            advance state;
            TyBool
        | Some { node = Ident "char"; _ } ->
            advance state;
            TyChar
        | Some { node = Ident "unit"; _ } ->
            advance state;
            TyUnit
        | Some { node = Ident s; _ } ->
            advance state;
            TyIdent s
        | Some { node; span } ->
            failwith
              (Printf.sprintf
                 "Unexpected token in type: %s at %s"
                 (show_token node) (show_span span))
        | None -> failwith "Expected type but found EOF"

let parse_param state =
    let name = eat_ident state in
        expect state Colon;
        let ty = parse_ty state in
            (name, ty)

let parse_param_list state =
    let rec loop acc =
        match peek state with
            | Some { node = RParen; _ } -> List.rev acc
            | _ -> (
                let param = parse_param state in
                    match peek state with
                        | Some { node = Comma; _ } ->
                            advance state;
                            loop (param :: acc)
                        | Some { node = RParen; _ } ->
                            loop (param :: acc)
                        | Some { node; span } ->
                            failwith
                              (Printf.sprintf
                                 "Expected , or ) but got \
                                  %s at %s"
                                 (show_token node)
                                 (show_span span))
                        | None ->
                            failwith
                              "Unexpected EOF in parameter \
                               list")
    in
        loop []

(* Operator precedence table *)
let precedence = function
    | Token.Star | Token.Slash -> 5
    | Token.Plus | Token.Minus -> 4
    | Token.Lt | Token.Le | Token.Gt | Token.Ge -> 3
    | Token.EqEq -> 2
    | Token.AndAnd | Token.OrOr -> 1
    | _ -> 0

let token_to_bin_op = function
    | Token.Plus -> Add
    | Token.Minus -> Sub
    | Token.Star -> Mul
    | Token.Slash -> Div
    | Token.EqEq -> Eq
    | Token.NotEq -> Neq
    | Token.Lt -> Lt
    | Token.Le -> Le
    | Token.Gt -> Gt
    | Token.Ge -> Ge
    | Token.AndAnd -> And
    | Token.OrOr -> Or
    | t ->
        failwith
          (Printf.sprintf "Not a binary operator: %s"
             (show_token t))

let rec parse_expr state = parse_binary_expr state 0

and parse_argument_list state =
    match peek state with
        | Some { node = RParen; _ } ->
            advance state;
            []
        | _ ->
            let rec loop acc =
                let arg = parse_expr state in
                    match peek state with
                        | Some { node = Comma; _ } ->
                            advance state;
                            loop (arg :: acc)
                        | Some { node = RParen; _ } ->
                            advance state;
                            List.rev (arg :: acc)
                        | Some { node; span } ->
                            failwith
                              (Printf.sprintf
                                 "Expected ',' or ')' but \
                                  got %s at %s"
                                 (show_token node)
                                 (show_span span))
                        | None ->
                            failwith
                              "Unexpected EOF in argument \
                               list"
            in
                loop []

and parse_primary_expr state =
    let rec parse_postfix e =
        match peek state with
            | Some { node = LParen; _ } ->
                advance state;
                let args = parse_argument_list state in
                    parse_postfix (Call (e, args))
            | _ -> e
    in
    let base =
        match peek state with
            | Some { node = Int i; _ } ->
                advance state;
                Int i
            | Some { node = Bool b; _ } ->
                advance state;
                Bool b
            | Some { node = Char c; _ } ->
                advance state;
                Char c
            | Some { node = Ident name; _ } ->
                advance state;
                Ident name
            | Some { node = LParen; _ } ->
                advance state;
                let e = parse_expr state in
                    expect state RParen;
                    e
            | Some { node; span } ->
                failwith
                  (Printf.sprintf
                     "Unexpected token in expression: %s \
                      at %s"
                     (show_token node) (show_span span))
            | None ->
                failwith "Unexpected EOF in expression"
    in
        parse_postfix base

and parse_binary_expr state min_prec =
    let lhs = parse_primary_expr state in
        parse_binary_rhs state min_prec lhs

and parse_binary_rhs state min_prec lhs =
    match peek state with
        | Some
            {
              node =
                ( Plus
                | Minus
                | Star
                | Slash
                | EqEq
                | NotEq
                | Lt
                | Le
                | Gt
                | Ge
                | AndAnd
                | OrOr ) as op_tok;
              _;
            } ->
            let prec = precedence op_tok in
                if prec < min_prec then
                  lhs
                else (
                  advance state;
                  let rhs =
                      parse_binary_expr state (prec + 1)
                  in
                  let binop = token_to_bin_op op_tok in
                  let combined = Binary (lhs, binop, rhs) in
                      parse_binary_rhs state min_prec
                        combined)
        | _ -> lhs

let parse_let state =
    expect state (Keyword K_let);
    let name = eat_ident state in
        expect state Assign;
        let value = parse_expr state in
            expect state Semicolon;
            Let (name, value)

let parse_return state =
    expect state (Keyword K_return);
    let value = parse_expr state in
        expect state Semicolon;
        Return value

let parse_stmt state =
    match peek state with
        | Some { node = Keyword K_let; _ } ->
            parse_let state
        | Some { node = Keyword K_return; _ } ->
            parse_return state
        | Some _ ->
            let e = parse_expr state in
                expect state Semicolon;
                Expr e
        | None ->
            failwith "Unexpected end of input in statement"

let parse_block state =
    expect state LBrace;
    let rec loop acc =
        match peek state with
            | Some { node = RBrace; _ } ->
                advance state;
                List.rev acc
            | Some _ ->
                let stmt = parse_stmt state in
                    loop (stmt :: acc)
            | None -> failwith "Unexpected EOF in block"
    in
        loop []

let parse_func state =
    expect state (Keyword K_fn);
    let name = eat_ident state in
        expect state LParen;
        let params = parse_param_list state in
            expect state RParen;
            expect state Arrow;
            let return_ty = parse_ty state in
            let body = parse_block state in
                { name; params; return_ty; body }

let parse_program tokens =
    let state = init_parser tokens in
    let rec loop acc =
        match peek state with
            | Some { node = Keyword K_fn; _ } ->
                let f = parse_func state in
                    loop (f :: acc)
            | Some { node; span } ->
                failwith
                  (Printf.sprintf
                     "Expected function definition but got \
                      %s at %s"
                     (show_token node) (show_span span))
            | None -> List.rev acc
    in
        loop []
