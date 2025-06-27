(* === Types === *)
type ty =
  | TyInt
  | TyFloat
  | TyBool
  | TyChar
  | TyUnit
  | TyIdent of string

(* === Function Parameters === *)
type param = string * ty

(* === Expressions === *)
type expr =
  | Int of int
  | Bool of bool
  | Char of char
  | Ident of string

(* === Statements === *)
type stmt =
  | Let of string * expr
  | Return of expr
  | Expr of expr

(* === Function Body === *)
type block = stmt list

(* === Function Definitions === *)
type func = {
  name : string;
  params : param list;
  return_ty : ty;
  body : block;
}

(* === Whole Program === *)
type program = func list

(* === Pretty Printing === *)
val string_of_ty : ty -> string
val string_of_expr : expr -> string
val string_of_stmt : stmt -> string
val string_of_param : param -> string
val string_of_func : func -> string
val string_of_program : program -> string
