%{
open Ast
%}

(* Token definitions *)
%token FN LET MUT IF ELSE WHILE FOR LOOP BREAK CONTINUE RETURN MATCH
%token STRUCT ENUM IMPL TRAIT USE MOD PUB CONST STATIC TRUE FALSE
%token AS TYPE IN SIZEOF NULL SELF USIZE
%token <string> IDENTIFIER
%token <int * Ast.integer_suffix option> INTEGER
%token <float * Ast.float_suffix option> FLOAT
%token <string> STRING
%token <char> CHAR
%token PLUS MINUS STAR SLASH PERCENT
%token PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN PERCENT_ASSIGN
%token EQ NE LT GT LE GE AND OR NOT
%token BIT_AND BIT_OR BIT_XOR SHL SHR
%token BIT_AND_ASSIGN BIT_OR_ASSIGN BIT_XOR_ASSIGN SHL_ASSIGN SHR_ASSIGN
%token ASSIGN ARROW FAT_ARROW DOT COMMA SEMICOLON COLON QUESTION DOTDOT DOUBLE_COLON UNDERSCORE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EOF

(* Precedence and associativity *)
%left OR
%left AND
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left EQ NE
%left LT GT LE GE
%left SHL SHR
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT NEG DEREF REF SIZEOF
%left DOT
%left LBRACKET
%left LPAREN
%right IF WHILE FOR

(* Start symbol *)
%start program
%type <Ast.program> program

%%

(* Program *)
program:
| items = item* EOF { items }

(* Items *)
item:
| func = function_def { Function func }
| struct_def = struct_def { Struct struct_def }
| enum_def = enum_def { Enum enum_def }
| trait_def = trait_def { Trait trait_def }
| impl_def = impl_def { Impl impl_def }
| global_var = global_var { global_var }
| type_alias = type_alias { type_alias }
| use_decl = use_decl { use_decl }
| mod_decl = mod_decl { mod_decl }
| global_let = global_let { global_let }

(* Function definition *)
function_def:
| vis = visibility FN name = IDENTIFIER generics = generic_params?
  LPAREN params = separated_list(COMMA, param) RPAREN
  return_type = return_type? body = block
  {
    { func_vis = vis;
      func_name = name;
      func_generics = (match generics with Some g -> g | None -> []);
      func_params = params;
      func_return = return_type;
      func_body = body }
  }

(* Struct definition *)
struct_def:
| vis = visibility STRUCT name = IDENTIFIER generics = generic_params?
  LBRACE fields = separated_list(COMMA, field_def) RBRACE
  {
    { struct_vis = vis;
      struct_name = name;
      struct_generics = (match generics with Some g -> g | None -> []);
      struct_fields = fields }
  }

(* Enum definition *)
enum_def:
| vis = visibility ENUM name = IDENTIFIER generics = generic_params?
  LBRACE variants = separated_list(COMMA, enum_variant) RBRACE
  {
    { enum_vis = vis;
      enum_name = name;
      enum_generics = (match generics with Some g -> g | None -> []);
      enum_variants = variants }
  }

(* Trait definition *)
trait_def:
| vis = visibility TRAIT name = IDENTIFIER generics = generic_params?
  LBRACE items = trait_item* RBRACE
  {
    { trait_vis = vis;
      trait_name = name;
      trait_generics = (match generics with Some g -> g | None -> []);
      trait_items = items }
  }

(* Impl definition *)
impl_def:
| IMPL generics = generic_params? target = plato_type
  LBRACE items = impl_item* RBRACE
  {
    { impl_generics = (match generics with Some g -> g | None -> []);
      impl_trait = None;
      impl_type = target;
      impl_items = items }
  }
| IMPL generics = generic_params? trait_path = path FOR target = plato_type
  LBRACE items = impl_item* RBRACE
  {
    { impl_generics = (match generics with Some g -> g | None -> []);
      impl_trait = Some trait_path;
      impl_type = target;
      impl_items = items }
  }

(* Global variable *)
global_var:
| vis = visibility is_const = const_or_static is_mut = MUT? name = IDENTIFIER
  type_ann = type_annotation? ASSIGN expr = expression SEMICOLON
  {
    GlobalVar (vis, is_const, (is_mut <> None), name, type_ann, expr)
  }

(* Global let (simplified global variable without static/const) *)
global_let:
| vis = visibility LET is_mut = MUT? name = IDENTIFIER 
  type_ann = type_annotation? ASSIGN expr = expression SEMICOLON
  {
    GlobalVar (vis, false, (is_mut <> None), name, type_ann, expr)
  }

(* Type alias *)
type_alias:
| vis = visibility TYPE name = IDENTIFIER generics = generic_params?
  ASSIGN ty = plato_type SEMICOLON
  {
    TypeAlias (vis, name, (match generics with Some g -> g | None -> []), ty)
  }

(* Helper rules for common patterns *)
type_annotation:
| COLON ty = plato_type { ty }

bounds:
| COLON bound_list = separated_list(PLUS, path) { bound_list }

(* Use declaration *)
use_decl:
| vis = visibility USE path = path SEMICOLON
  { Use (vis, path) }

(* Module declaration *)
mod_decl:
| vis = visibility MOD name = IDENTIFIER SEMICOLON
  { Mod (vis, name, None) }
| vis = visibility MOD name = IDENTIFIER LBRACE items = item* RBRACE
  { Mod (vis, name, Some items) }

(* Visibility *)
visibility:
| (* empty *) { Private }
| PUB { Public }

(* Const or static *)
const_or_static:
| CONST { true }
| STATIC { false }

(* Generic parameters *)
generic_params:
| LT params = separated_list(COMMA, generic_param) GT { params }

generic_param:
| name = IDENTIFIER bounds = bounds?
  { (name, bounds) }

(* Field definition *)
field_def:
| vis = visibility name = IDENTIFIER COLON ty = plato_type
  {
    { field_vis = vis;
      field_name = name;
      field_type = ty }
  }

(* Enum variant *)
enum_variant:
| name = IDENTIFIER data = variant_data? value = variant_value?
  {
    { variant_name = name;
      variant_data = data;
      variant_value = value }
  }

variant_data:
| LPAREN types = separated_list(COMMA, plato_type) RPAREN { types }

variant_value:
| ASSIGN i = INTEGER { let (value, _) = i in value }

(* Parameter *)
param:
| name = IDENTIFIER COLON ty = plato_type
  {
    { param_name = name;
      param_type = ty }
  }

(* Return type *)
return_type:
| ARROW ty = plato_type { ty }

(* Trait item *)
trait_item:
| FN name = IDENTIFIER generics = generic_params?
  LPAREN params = separated_list(COMMA, param) RPAREN
  return_type = return_type? SEMICOLON
  {
    TraitFunction (name, (match generics with Some g -> g | None -> []), params, return_type)
  }
| TYPE name = IDENTIFIER bounds = bounds? SEMICOLON
  { AssociatedType (name, bounds) }

(* Impl item *)
impl_item:
| func = function_def { ImplFunction func }
| TYPE name = IDENTIFIER ASSIGN ty = plato_type SEMICOLON
  { ImplTypeAlias (name, ty) }

(* Types *)
plato_type:
| path = path generics = type_generics?
  { 
    (* Convert single-identifier primitive types to PrimType *)
    match path with
    | [name] -> (
      match name with
      | "i8" -> PrimType I8 | "i16" -> PrimType I16 | "i32" -> PrimType I32 | "i64" -> PrimType I64
      | "u8" -> PrimType U8 | "u16" -> PrimType U16 | "u32" -> PrimType U32 | "u64" -> PrimType U64
      | "usize" -> PrimType Usize | "f32" -> PrimType F32 | "f64" -> PrimType F64
      | "bool" -> PrimType Bool | "char" -> PrimType Char | "str" -> PrimType Str | "void" -> PrimType Void
      | "int" -> PrimType I32  (* Add int as alias for i32 *)
      | _ -> PathType (path, generics)
    )
    | _ -> PathType (path, generics)
  }
| LBRACKET ty = plato_type SEMICOLON size = expression RBRACKET
  { ArrayType (ty, size) }
| STAR ty = plato_type { PointerType ty }
| FN LPAREN params = separated_list(COMMA, plato_type) RPAREN
  return_type = return_type?
  { FunctionType (params, return_type) }

type_generics:
| LT types = separated_list(COMMA, plato_type) GT { types }

(* Path *)
path:
| id = IDENTIFIER { [id] }
| path = path DOUBLE_COLON id = IDENTIFIER { path @ [id] }

(* Expressions - simplified version *)
simple_expression:
| lit = literal { Literal lit }
| id = IDENTIFIER { Identifier id }
| LPAREN e = expression RPAREN { e }

expression:
| e = simple_expression { e }
| e1 = expression op = binary_op e2 = expression { BinaryOp (e1, op, e2) }
| op = unary_op e = expression %prec NOT { UnaryOp (op, e) }
| e = expression AS ty = plato_type { Cast (e, ty) }
| e1 = expression LBRACKET e2 = expression RBRACKET { Index (e1, e2) }
| e = expression DOT field = IDENTIFIER { FieldAccess (e, field) }
| e = expression ARROW field = IDENTIFIER { PointerAccess (e, field) }
| func = expression LPAREN args = separated_list(COMMA, expression) RPAREN
  { FunctionCall (func, args) }
| LBRACKET exprs = separated_list(COMMA, expression) RBRACKET
  { ArrayExpr exprs }
| path = path LBRACE fields = separated_list(COMMA, struct_field) RBRACE
  { StructExpr (path, fields) }
| IF cond = condition_expr then_block = block ELSE else_block = block %prec IF
  { If (cond, then_block, Some else_block) }
| MATCH expr = expression LBRACE arms = separated_nonempty_list(COMMA, match_arm) RBRACE
  { Match (expr, arms) }
| LOOP body = block { Loop body }
| WHILE cond = condition_expr body = block %prec WHILE { While (cond, body) }
| FOR var = IDENTIFIER IN iter = condition_expr body = block %prec FOR
  { For (var, iter, body) }
| RETURN expr = expression? { Return expr }
| BREAK expr = expression? { Break expr }
| CONTINUE { Continue }

else_clause:
| ELSE block = block { block }

(* Literals *)
literal:
| i = INTEGER { IntLit (fst i, snd i) }
| f = FLOAT { FloatLit (fst f, snd f) }
| s = STRING { StringLit s }
| c = CHAR { CharLit c }
| TRUE { BoolLit true }
| FALSE { BoolLit false }
| LPAREN RPAREN { UnitLit }
| NULL { NullLit }

(* Binary operators *)
binary_op:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mul }
| SLASH { Div }
| PERCENT { Mod }
| EQ { Eq }
| NE { Ne }
| LT { Lt }
| GT { Gt }
| LE { Le }
| GE { Ge }
| AND { And }
| OR { Or }
| BIT_AND { BitAnd }
| BIT_OR { BitOr }
| BIT_XOR { BitXor }
| SHL { Shl }
| SHR { Shr }

(* Unary operators *)
unary_op:
| NOT { Not }
| MINUS %prec NEG { Neg }
| STAR %prec DEREF { Deref }
| BIT_AND %prec REF { Ref }
| SIZEOF { Sizeof }

(* Assignment operators *)
assign_op:
| ASSIGN { Assign }
| PLUS_ASSIGN { AddAssign }
| MINUS_ASSIGN { SubAssign }
| STAR_ASSIGN { MulAssign }
| SLASH_ASSIGN { DivAssign }
| PERCENT_ASSIGN { ModAssign }
| BIT_AND_ASSIGN { BitAndAssign }
| BIT_OR_ASSIGN { BitOrAssign }
| BIT_XOR_ASSIGN { BitXorAssign }
| SHL_ASSIGN { ShlAssign }
| SHR_ASSIGN { ShrAssign }

(* Struct field initialization *)
struct_field:
| name = IDENTIFIER COLON expr = expression { (name, expr) }

(* Match arms *)
match_arm:
| pattern = pattern FAT_ARROW expr = expression
  { MatchArm (pattern, expr) }

(* Patterns *)
pattern:
| lit = literal { LiteralPattern lit }
| UNDERSCORE { WildcardPattern }
| id = IDENTIFIER { IdentifierPattern id }
| id = IDENTIFIER DOUBLE_COLON rest = path LPAREN patterns = separated_list(COMMA, pattern) RPAREN
  { EnumPattern (id :: rest, Some patterns) }
| id = IDENTIFIER DOUBLE_COLON rest = path { EnumPattern (id :: rest, None) }
| LPAREN patterns = separated_list(COMMA, pattern) RPAREN
  { TuplePattern patterns }

(* Statements *)
statement:
| LET is_mut = MUT? name = IDENTIFIER type_ann = type_annotation?
  init = initer? SEMICOLON
  { LetStmt ((is_mut <> None), name, type_ann, init) }
| lval = lvalue op = assign_op expr = expression SEMICOLON
  { AssignStmt (lval, op, expr) }
| expr = expression SEMICOLON { ExprStmt expr }
| item = item { ItemStmt item }

initer:
| ASSIGN expr = expression { expr }

(* Lvalues *)
lvalue:
| id = IDENTIFIER { LvalueId id }
| STAR lval = lvalue { LvalueDeref lval }
| lval = lvalue LBRACKET expr = expression RBRACKET
  { LvalueIndex (lval, expr) }
| lval = lvalue DOT field = IDENTIFIER { LvalueField (lval, field) }
| lval = lvalue ARROW field = IDENTIFIER { LvaluePointer (lval, field) }

(* Blocks - restructured to avoid ambiguity *)
block:
| LBRACE RBRACE { ([], None) }
| LBRACE inner = block_inner RBRACE { inner }

block_inner:
| stmt = statement { ([stmt], None) }
| stmt = statement inner = block_inner { let (stmts, expr) = inner in (stmt::stmts, expr) }
| expr = expression { ([], Some expr) }
| if_stmt = if_statement inner = block_inner { let (stmts, expr) = inner in (if_stmt::stmts, expr) }
| if_stmt = if_statement { ([if_stmt], None) }

(* IF statements without else clause - treated as statements *)
if_statement:
| IF cond = condition_expr then_block = block
  { ExprStmt (If (cond, then_block, None)) }

(* Condition expressions - handles identifiers explicitly to avoid conflicts *)
condition_expr:
| lit = literal { Literal lit }
| id = IDENTIFIER { Identifier id }
| LPAREN e = expression RPAREN { e }
| e1 = condition_expr op = binary_op e2 = condition_expr { BinaryOp (e1, op, e2) }
| op = unary_op e = condition_expr { UnaryOp (op, e) }
| func = condition_expr LPAREN args = separated_list(COMMA, expression) RPAREN
  { FunctionCall (func, args) }
| e = condition_expr DOT field = IDENTIFIER { FieldAccess (e, field) }
| e1 = condition_expr LBRACKET e2 = expression RBRACKET { Index (e1, e2) }
| LBRACKET exprs = separated_list(COMMA, expression) RBRACKET
  { ArrayExpr exprs }

%%
