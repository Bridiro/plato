open Token
open Ast

exception ParserError of string * span option

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

let peek_next_two state =
    match peek state with
        | None -> None
        | Some first -> (
            advance state;
            let second = peek state in
                state.pos <- state.pos - 1;
                (* rewind *)
                    match second with
                    | Some s -> Some (first, s)
                    | None -> None)

let expect state expected =
    match peek state with
        | Some { node; span = _ } when node = expected ->
            advance state
        | Some { node; span } ->
            raise
              (ParserError
                 ( "Expected " ^ show_token expected
                   ^ " but got " ^ show_token node,
                   Some span ))
        | None ->
            raise
              (ParserError ("Unexpected end of input", None))

let eat_ident state =
    match peek state with
        | Some { node = Ident name; _ } ->
            advance state;
            name
        | Some { node; span } ->
            raise
              (ParserError
                 ( "Expected identifier but got "
                   ^ show_token node,
                   Some span ))
        | None ->
            raise
              (ParserError
                 ("Expected identifier but got EOF", None))

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
            raise
              (ParserError
                 ( "Unexpected token in type "
                   ^ show_token node,
                   Some span ))
        | None ->
            raise
              (ParserError
                 ("Expected type but got EOF", None))

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
                            raise
                              (ParserError
                                 ( "Expected ',' or ')' \
                                    but got "
                                   ^ show_token node,
                                   Some span ))
                        | None ->
                            raise
                              (ParserError
                                 ( "Unexpected EOF in \
                                    parameter list",
                                   None )))
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
        raise
          (ParserError
             ("Not a binary operator: " ^ show_token t, None))

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
                            raise
                              (ParserError
                                 ( "Expected ',' or ')' \
                                    but got "
                                   ^ show_token node,
                                   Some span ))
                        | None ->
                            raise
                              (ParserError
                                 ( "Unexpected EOF in \
                                    parameter list",
                                   None ))
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
                raise
                  (ParserError
                     ( "Unexpected token in expression "
                       ^ show_token node,
                       Some span ))
            | None ->
                raise
                  (ParserError
                     ("Unexpected EOF in expression", None))
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
    let ty =
        match peek state with
            | Some { node = Colon; _ } ->
                advance state;
                Some (parse_ty state)
            | _ -> None
    in
        expect state Assign;
        let value = parse_expr state in
            expect state Semicolon;
            Let (name, ty, value)

let parse_return state =
    expect state (Keyword K_return);
    let value = parse_expr state in
        expect state Semicolon;
        Return value

let rec parse_if_stmt state =
    expect state (Keyword K_if);
    let cond = parse_expr state in
    let then_block = parse_block state in
    let else_block =
        match peek state with
            | Some { node = Keyword K_else; _ } ->
                advance state;
                Some (parse_block state)
            | _ -> None
    in
        If (cond, then_block, else_block)

and parse_stmt state =
    match peek state with
        | Some { node = Keyword K_let; _ } ->
            parse_let state
        | Some { node = Keyword K_return; _ } ->
            parse_return state
        | Some { node = Keyword K_if; _ } ->
            parse_if_stmt state
        | Some { node = Ident name; _ } -> (
            (* lookahead to see if this is an assignment *)
            advance state;
            match peek state with
                | Some { node = Assign; _ } ->
                    advance state;
                    let value = parse_expr state in
                        expect state Semicolon;
                        Assign (name, value)
                | Some { node; span } ->
                    raise
                      (ParserError
                         ( "Unexpected token after \
                            identifier " ^ show_token node,
                           Some span ))
                | None ->
                    raise
                      (ParserError
                         ( "Unexpected EOF after identifier",
                           None )))
        | Some _ ->
            let e = parse_expr state in
                expect state Semicolon;
                Expr e
        | None ->
            raise
              (ParserError
                 ("Unexpected end of input stream", None))

and is_start_of_expr = function
    | Token.Ident _
    | Token.Int _
    | Token.Float _
    | Token.Bool _
    | Token.Char _
    | Keyword K_if
    | Keyword K_true
    | Keyword K_false
    | LParen -> true
    | _ -> false

and parse_block state : block =
    expect state LBrace;
    let rec collect acc =
        match peek state with
            | Some { node = RBrace; _ } ->
                advance state;
                List.rev acc
            | _ ->
                let stmt =
                    match peek_next_two state with
                        | Some
                            ( expr_token,
                              { node = Semicolon; _ } )
                          when is_start_of_expr
                                 expr_token.node ->
                            let e = parse_expr state in
                                expect state Semicolon;
                                Expr e
                        | Some
                            ( expr_token,
                              { node = RBrace; _ } )
                          when is_start_of_expr
                                 expr_token.node ->
                            let e = parse_expr state in
                                ExprValue e
                        | _ -> parse_stmt state
                in
                    collect (stmt :: acc)
    in
        collect []

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
                    loop (Func f :: acc)
            | Some { node = Keyword K_let; _ } -> (
                let stmt = parse_let state in
                    match stmt with
                        | Let (name, Some ty, value) ->
                            loop
                              (GlobalLet
                                 (name, Some ty, value)
                              :: acc)
                        | Let (name, None, value) ->
                            loop
                              (GlobalLet (name, None, value)
                              :: acc)
                        | _ ->
                            raise
                              (ParserError
                                 ( "Expected let \
                                    declaration at top \
                                    level",
                                   None )))
            | Some { node; span } ->
                raise
                  (ParserError
                     ( "Unexpected token at top level "
                       ^ show_token node,
                       Some span ))
            | None -> List.rev acc
    in
        loop []
