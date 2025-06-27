type expr =
  | Int of int
  | Bool of bool
  | Char of char
  | Ident of string

type stmt =
  | Let of string * expr
  | Return of expr
  | Expr of expr

type program = stmt list
