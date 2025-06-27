open Token
open Ast

type parser_state = {
  tokens : located_token array;
  mutable pos : int;
}

let init_parser tokens = { tokens = Array.of_list tokens; pos = 0 }

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
           (show_token expected) (show_token node) (show_span span))
  | None -> failwith "Unexpected end of input"

let eat_ident state =
  match peek state with
  | Some { node = Ident name; _ } -> advance state; name
  | Some { node; span } ->
      failwith
        (Printf.sprintf "Expected identifier but got %s at %s"
           (show_token node) (show_span span))
  | None -> failwith "Expected identifier but got EOF"

let parse_expr state =
  match peek state with
  | Some { node = Int i; _ } -> advance state; Int i
  | Some { node = Bool b; _ } -> advance state; Bool b
  | Some { node = Char c; _ } -> advance state; Char c
  | Some { node = Ident name; _ } -> advance state; Ident name
  | Some { node; span } ->
      failwith
        (Printf.sprintf "Unexpected token in expression: %s at %s"
           (show_token node) (show_span span))
  | None -> failwith "Unexpected end of input in expression"

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
  | Some { node = Keyword K_let; _ } -> parse_let state
  | Some { node = Keyword K_return; _ } -> parse_return state
  | Some _ ->
      let e = parse_expr state in
      expect state Semicolon;
      Expr e
  | None -> failwith "Unexpected end of input in statement"

let parse_program tokens =
  let state = init_parser tokens in
  let rec loop acc =
    match peek state with
    | Some _ ->
        let stmt = parse_stmt state in
        loop (stmt :: acc)
    | None -> List.rev acc
  in
  loop []
