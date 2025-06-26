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
