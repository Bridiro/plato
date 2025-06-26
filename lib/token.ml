type position = {
  line : int;
  column : int;
}

type span = {
  start_pos : position;
  end_pos : position;
}

type 'a located = {
  node : 'a;
  span : span;
}

type keyword =
  | K_fn
  | K_let
  | K_if
  | K_else
  | K_return
  | K_true
  | K_false

type token =
  | Int of int
  | Float of float
  | Char of char
  | Bool of bool
  | Unit
  | Ident of string
  | Keyword of keyword
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Comma
  | Colon
  | Semicolon
  | Arrow
  | Plus
  | Minus
  | Star
  | Slash
  | EqEq
  | NotEq
  | Lt
  | Gt
  | Le
  | Ge
  | Assign

type located_token = token located

let string_of_keyword = function
    | K_fn -> "fn"
    | K_let -> "let"
    | K_if -> "if"
    | K_else -> "else"
    | K_return -> "return"
    | K_true -> "true"
    | K_false -> "false"

let show_token = function
    | Int i -> Printf.sprintf "Int(%d)" i
    | Float f -> Printf.sprintf "Float(%f)" f
    | Char c -> Printf.sprintf "Char('%c')" c
    | Bool b -> Printf.sprintf "Bool(%b)" b
    | Unit -> "Unit"
    | Ident s -> Printf.sprintf "Ident(%s)" s
    | Keyword kw ->
        Printf.sprintf "Keyword(%s)" (string_of_keyword kw)
    | LParen -> "LParen"
    | RParen -> "RParen"
    | LBrace -> "LBrace"
    | RBrace -> "RBrace"
    | Comma -> "Comma"
    | Colon -> "Colon"
    | Semicolon -> "Semicolon"
    | Arrow -> "Arrow"
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Star -> "Star"
    | Slash -> "Slash"
    | EqEq -> "EqEq"
    | NotEq -> "NotEq"
    | Lt -> "Lt"
    | Gt -> "Gt"
    | Le -> "Le"
    | Ge -> "Ge"
    | Assign -> "Assign"

let show_position (pos : position) =
    Printf.sprintf "%d:%d" pos.line pos.column

let show_span (span : span) =
    Printf.sprintf "%s - %s"
      (show_position span.start_pos)
      (show_position span.end_pos)
