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

(* === Binary Operators === *)
type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or

(* === Expressions === *)
type expr =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | Unit
  | Ident of string
  | Binary of expr * bin_op * expr
  | Call of expr * expr list

(* === Statements === *)
and stmt =
  | Let of string * expr
  | Return of expr
  | Expr of expr
  | ExprValue of expr
  | Assign of string * expr
  | If of expr * block * block option

(* === Function Body === *)
and block = stmt list

(* === Function Definitions === *)
type func = {
  name : string;
  params : param list;
  return_ty : ty;
  body : block;
}

(* === GLobal declaration definition === *)
type decl =
  | GlobalLet of string * expr
  | Func of func

(* === Whole Program === *)
type program = decl list

(* === Pretty Printing === *)
val string_of_ty : ty -> string
val string_of_bin_op : bin_op -> string
val string_of_expr : expr -> string
val string_of_stmt : stmt -> string
val string_of_param : param -> string
val string_of_func : func -> string
val string_of_decl : decl -> string
val string_of_program : program -> string
