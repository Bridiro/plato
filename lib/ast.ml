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
let string_of_ty = function
    | TyInt -> "int"
    | TyFloat -> "float"
    | TyBool -> "bool"
    | TyChar -> "char"
    | TyUnit -> "unit"
    | TyIdent id -> id

let string_of_expr = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Char c -> Printf.sprintf "'%c'" c
    | Ident s -> s

let string_of_stmt = function
    | Let (name, e) ->
        Printf.sprintf "let %s = %s;" name
          (string_of_expr e)
    | Return e ->
        Printf.sprintf "return %s;" (string_of_expr e)
    | Expr e -> Printf.sprintf "%s;" (string_of_expr e)

let string_of_param (name, ty) =
    Printf.sprintf "%s: %s" name (string_of_ty ty)

let string_of_func f =
    let params =
        String.concat ", "
          (List.map string_of_param f.params)
    in
    let body =
        String.concat "\n  "
          (List.map string_of_stmt f.body)
    in
        Printf.sprintf "fn %s(%s) -> %s {\n  %s\n}" f.name
          params
          (string_of_ty f.return_ty)
          body

let string_of_program p =
    String.concat "\n\n" (List.map string_of_func p)
