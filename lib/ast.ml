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
  | Bool of bool
  | Char of char
  | Ident of string
  | Binary of expr * bin_op * expr
  | If of expr * block * block
  | Call of expr * expr list

(* === Statements === *)
and stmt =
  | Let of string * expr
  | Return of expr
  | Expr of expr

(* === Function Body === *)
and block = stmt list

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

let string_of_bin_op = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
    | And -> "&&"
    | Or -> "||"

let rec string_of_expr = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Char c -> Printf.sprintf "'%c'" c
    | Ident s -> s
    | Binary (lhs, op, rhs) ->
        Printf.sprintf "(%s %s %s)"
          (string_of_expr lhs)
          (string_of_bin_op op)
          (string_of_expr rhs)
    | If (cond, then_b, else_b) ->
        Printf.sprintf "if %s { %s } else { %s }"
          (string_of_expr cond)
          (String.concat "; "
             (List.map string_of_stmt then_b))
          (String.concat "; "
             (List.map string_of_stmt else_b))
    | Call (fn, args) ->
        Printf.sprintf "%s(%s)" (string_of_expr fn)
          (String.concat ", "
             (List.map string_of_expr args))

and string_of_stmt = function
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
