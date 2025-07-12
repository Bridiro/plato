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

type plato_type =
  | PrimType of primitive_type
  | ArrayType of plato_type * expression
  | PointerType of plato_type
  | FunctionType of plato_type list * plato_type option
  | PathType of path * plato_type list option
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
  | Literal of literal
  | Identifier of string
  | PathExpr of path
  | BinaryOp of expression * binary_op * expression
  | UnaryOp of unary_op * expression
  | Cast of expression * plato_type
  | Index of expression * expression
  | FieldAccess of expression * string
  | PointerAccess of expression * string
  | FunctionCall of expression * expression list
  | ArrayExpr of expression list
  | StructExpr of path * (string * expression) list
  | Block of block
  | If of expression * block * block option
  | Match of expression * match_arm list
  | Loop of block
  | While of expression * block
  | For of string * expression * block
  | Return of expression option
  | Break of expression option
  | Continue

and match_arm = MatchArm of pattern * expression

and pattern =
  | LiteralPattern of literal
  | IdentifierPattern of string
  | WildcardPattern
  | EnumPattern of path * pattern list option
  | TuplePattern of pattern list

and statement =
  | LetStmt of bool * string * plato_type option * expression option
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
  field_type : plato_type;
}

and enum_variant = {
  variant_name : string;
  variant_data : plato_type list option;
  variant_value : int option;
}

and param = {
  param_name : string;
  param_type : plato_type;
}

and function_def = {
  func_vis : visibility;
  func_name : string;
  func_generics : generic_param list;
  func_params : param list;
  func_return : plato_type option;
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
      string * generic_param list * param list * plato_type option
  | AssociatedType of string * path list option

and trait_def = {
  trait_vis : visibility;
  trait_name : string;
  trait_generics : generic_param list;
  trait_items : trait_item list;
}

and impl_item =
  | ImplFunction of function_def
  | ImplTypeAlias of string * plato_type

and impl_def = {
  impl_generics : generic_param list;
  impl_trait : path option;
  impl_type : plato_type;
  impl_items : impl_item list;
}

and item =
  | Function of function_def
  | Struct of struct_def
  | Enum of enum_def
  | Trait of trait_def
  | Impl of impl_def
  | GlobalVar of
      visibility * bool * bool * string * plato_type option * expression
  | TypeAlias of visibility * string * generic_param list * plato_type
  | Use of visibility * path
  | Mod of visibility * string * item list option

type program = item list
