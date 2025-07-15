type position = {
  line : int;
  column : int;
}

type 'a located = {
  loc : position;
  value : 'a;
}

type integer_suffix =
  | I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | Usize

type float_suffix =
  | F32
  | F64

type literal =
  | IntLit of int * integer_suffix option
  | FloatLit of float * float_suffix option
  | StringLit of string
  | CharLit of char
  | BoolLit of bool
  | UnitLit
  | NullLit

type path = string list

type primitive_type =
  | I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | Usize
  | F32
  | F64
  | Bool
  | Char
  | Str
  | Void

type eiron_type =
  | PrimType of primitive_type
  | ArrayType of eiron_type * expression
  | PointerType of eiron_type
  | FunctionType of eiron_type list * eiron_type option
  | PathType of path * eiron_type list option
  | GenericType of string

and binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor
  | Shl
  | Shr

and unary_op =
  | Not
  | Neg
  | Deref
  | Ref
  | Sizeof

and assign_op =
  | Assign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | BitAndAssign
  | BitOrAssign
  | BitXorAssign
  | ShlAssign
  | ShrAssign

and expression =
  | Literal of literal * position
  | Identifier of string * position
  | PathExpr of path * position
  | BinaryOp of expression * binary_op * expression * position
  | UnaryOp of unary_op * expression * position
  | Cast of expression * eiron_type * position
  | Index of expression * expression * position
  | FieldAccess of expression * string * position
  | PointerAccess of expression * string * position
  | FunctionCall of expression * expression list * position
  | ArrayExpr of expression list * position
  | StructExpr of path * (string * expression) list * position
  | Block of block * position
  | If of expression * block * block option * position
  | Match of expression * match_arm list * position
  | Loop of block * position
  | While of expression * block * position
  | For of string * expression * block * position
  | Return of expression option * position
  | Break of expression option * position
  | Continue of position
  | Range of expression * expression * position

and match_arm = MatchArm of pattern * expression

and pattern =
  | LiteralPattern of literal
  | IdentifierPattern of string
  | WildcardPattern
  | EnumPattern of path * pattern list option
  | TuplePattern of pattern list

and statement =
  | LetStmt of bool * string * eiron_type option * expression option
  | AssignStmt of lvalue * assign_op * expression
  | ExprStmt of expression
  | ItemStmt of item

and lvalue =
  | LvalueId of string
  | LvalueDeref of lvalue
  | LvalueIndex of lvalue * expression
  | LvalueField of lvalue * string
  | LvaluePointer of lvalue * string

and block = statement list * expression option

and visibility =
  | Public
  | Private

and generic_param = string * path list option

and field_def = {
  field_vis : visibility;
  field_name : string;
  field_type : eiron_type;
}

and enum_variant = {
  variant_name : string;
  variant_data : eiron_type list option;
  variant_value : int option;
}

and param = {
  param_name : string;
  param_type : eiron_type;
}

and function_def = {
  func_vis : visibility;
  func_name : string;
  func_generics : generic_param list;
  func_params : param list;
  func_return : eiron_type option;
  func_body : block;
}

and struct_def = {
  struct_vis : visibility;
  struct_name : string;
  struct_generics : generic_param list;
  struct_fields : field_def list;
}

and enum_def = {
  enum_vis : visibility;
  enum_name : string;
  enum_generics : generic_param list;
  enum_variants : enum_variant list;
}

and trait_item =
  | TraitFunction of
      string * generic_param list * param list * eiron_type option
  | AssociatedType of string * path list option

and trait_def = {
  trait_vis : visibility;
  trait_name : string;
  trait_generics : generic_param list;
  trait_items : trait_item list;
}

and impl_item =
  | ImplFunction of function_def
  | ImplTypeAlias of string * eiron_type

and impl_def = {
  impl_generics : generic_param list;
  impl_trait : path option;
  impl_type : eiron_type;
  impl_items : impl_item list;
}

and item =
  | Function of function_def
  | Struct of struct_def
  | Enum of enum_def
  | Trait of trait_def
  | Impl of impl_def
  | GlobalVar of
      visibility * bool * bool * string * eiron_type option * expression
  | TypeAlias of visibility * string * generic_param list * eiron_type
  | Use of visibility * path
  | Mod of visibility * string * item list option

type program = item list

(* Helper function to extract position from expressions *)
let get_expression_position = function
  | Literal (_, pos) -> pos
  | Identifier (_, pos) -> pos
  | PathExpr (_, pos) -> pos
  | BinaryOp (_, _, _, pos) -> pos
  | UnaryOp (_, _, pos) -> pos
  | Cast (_, _, pos) -> pos
  | Index (_, _, pos) -> pos
  | FieldAccess (_, _, pos) -> pos
  | PointerAccess (_, _, pos) -> pos
  | FunctionCall (_, _, pos) -> pos
  | ArrayExpr (_, pos) -> pos
  | StructExpr (_, _, pos) -> pos
  | Block (_, pos) -> pos
  | If (_, _, _, pos) -> pos
  | Match (_, _, pos) -> pos
  | Loop (_, pos) -> pos
  | While (_, _, pos) -> pos
  | For (_, _, _, pos) -> pos
  | Return (_, pos) -> pos
  | Break (_, pos) -> pos
  | Continue pos -> pos
  | Range (_, _, pos) -> pos
