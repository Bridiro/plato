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
    | Float f -> string_of_float f
    | Bool b -> string_of_bool b
    | Char c -> Printf.sprintf "'%c'" c
    | Unit -> "()"
    | Ident s -> s
    | Binary (lhs, op, rhs) ->
        Printf.sprintf "(%s %s %s)"
          (string_of_expr lhs)
          (string_of_bin_op op)
          (string_of_expr rhs)
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
    | Expr e ->
        Printf.sprintf "%s;" (string_of_expr e)
    | ExprValue e ->
        string_of_expr e
    | Assign (name, e) ->
        Printf.sprintf "%s = %s;" name (string_of_expr e)
    | If (cond, then_b, Some else_b) ->
        Printf.sprintf "if %s { %s } else { %s }"
          (string_of_expr cond)
          (String.concat "\n  "
             (List.map string_of_stmt then_b))
          (String.concat "\n  "
             (List.map string_of_stmt else_b))
    | If (cond, then_b, None) ->
        Printf.sprintf "if %s { %s }"
          (string_of_expr cond)
          (String.concat "\n  "
             (List.map string_of_stmt then_b))

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

let string_of_decl = function
    | GlobalLet (name, e) ->
        Printf.sprintf "let %s = %s;" name
          (string_of_expr e)
    | Func f -> string_of_func f

let string_of_program p =
    String.concat "\n\n" (List.map string_of_decl p)
