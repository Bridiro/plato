%{
open Ast

(* Helper function to create position from Menhir position *)
let make_position pos =
  { line = pos.Lexing.pos_lnum; 
    column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 }

(* Elegant position tracking pattern from the forum *)
let make_position_span start_pos _end_pos =
  { line = start_pos.Lexing.pos_lnum; 
    column = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1 }

(* Function to add position information to any AST node *)
let with_position startpos _endpos x =
  (x, make_position_span startpos startpos)
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
%token ASSIGN ARROW FAT_ARROW DOT COMMA SEMICOLON COLON DOUBLE_COLON UNDERSCORE DOTDOT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EOF

(* Precedence and associativity *)
%right ASSIGN
%left OR
%left AND
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left EQ NE
%left LT GT LE GE
%left DOTDOT
%left SHL SHR
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT
%left DOT
%left LBRACKET
%left LPAREN

(* Start symbol *)
%start program
%type <Ast.program> program

%%

(* Elegant position tracking pattern - can be used anywhere *)
%public %inline located(X):
| x = X { with_position $startpos $endpos x }

(* Enhanced positioned versions of key constructs *)
%public %inline positioned_binary_op(OP):
| op = OP { (op, make_position $startpos(op)) }

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
  LPAREN params = param_list RPAREN
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
  LBRACE fields = field_list RBRACE
  {
    { struct_vis = vis;
      struct_name = name;
      struct_generics = (match generics with Some g -> g | None -> []);
      struct_fields = fields }
  }

(* Enum definition *)
enum_def:
| vis = visibility ENUM name = IDENTIFIER generics = generic_params?
  LBRACE variants = variant_list RBRACE
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
| IMPL generics = generic_params? target = eiron_type
  LBRACE items = impl_item* RBRACE
  {
    { impl_generics = (match generics with Some g -> g | None -> []);
      impl_trait = None;
      impl_type = target;
      impl_items = items }
  }
| IMPL generics = generic_params? trait_path = path FOR target = eiron_type
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
  ASSIGN ty = eiron_type SEMICOLON
  {
    TypeAlias (vis, name, (match generics with Some g -> g | None -> []), ty)
  }

(* Helper rules for common patterns *)
type_annotation:
| COLON ty = eiron_type { ty }

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
| vis = visibility name = IDENTIFIER COLON ty = eiron_type
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
| LPAREN types = type_list RPAREN { types }

variant_value:
| ASSIGN i = INTEGER { let (value, _) = i in value }

(* Parameter *)
param:
| name = IDENTIFIER COLON ty = eiron_type
  {
    { param_name = name;
      param_type = ty }
  }
| SELF COLON ty = eiron_type
  {
    { param_name = "self";
      param_type = ty }
  }
| SELF
  {
    { param_name = "self";
      param_type = SelfType (make_position $startpos) }
  }

(* Return type *)
return_type:
| ARROW ty = eiron_type { ty }

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
| TYPE name = IDENTIFIER ASSIGN ty = eiron_type SEMICOLON
  { ImplTypeAlias (name, ty) }

(* Types *)
eiron_type:
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
      | _ -> PathType (path, generics)
    )
    | _ -> PathType (path, generics)
  }
| USIZE { PrimType Usize }
| LBRACKET ty = eiron_type SEMICOLON size = expression RBRACKET
  { ArrayType (ty, size) }
| STAR ty = eiron_type { PointerType ty }
| FN LPAREN params = separated_list(COMMA, eiron_type) RPAREN
  return_type = return_type?
  { FunctionType (params, return_type) }

type_generics:
| LT types = separated_list(COMMA, eiron_type) GT { types }

(* Path *)
path:
| id = IDENTIFIER { [id] }
| path = path DOUBLE_COLON id = IDENTIFIER { path @ [id] }

(* Expressions - simplified version *)
simple_expression:
| lit = literal { Literal (lit, make_position $startpos) }
| id = IDENTIFIER { Identifier (id, make_position $startpos) }
| SELF { Identifier ("self", make_position $startpos) }
| path = multi_part_path { PathExpr (path, make_position $startpos) }
| LPAREN e = expression RPAREN { e }
| block = block { Block (block, make_position $startpos) }

(* Multi-part paths (at least one ::) *)
multi_part_path:
| id1 = IDENTIFIER DOUBLE_COLON id2 = IDENTIFIER { [id1; id2] }
| path = multi_part_path DOUBLE_COLON id = IDENTIFIER { path @ [id] }

expression:
| e = simple_expression { e }
| e1 = expression op = binary_op e2 = expression { BinaryOp (e1, op, e2, make_position $startpos(op)) }
| op = unary_op e = expression %prec NOT { UnaryOp (op, e, make_position $startpos) }
| e = expression AS ty = eiron_type { Cast (e, ty, make_position $startpos) }
| e1 = expression LBRACKET e2 = expression RBRACKET { Index (e1, e2, make_position $startpos) }
| e = expression DOT field = IDENTIFIER { FieldAccess (e, field, make_position $startpos) }
| e = expression ARROW field = IDENTIFIER { PointerAccess (e, field, make_position $startpos) }
| func = expression LPAREN args = expression_list RPAREN
  { FunctionCall (func, args, make_position $startpos) }
| LBRACKET exprs = expression_list RBRACKET
  { ArrayExpr (exprs, make_position $startpos) }
| path = path LBRACE fields = struct_field_list RBRACE
  { StructExpr (path, fields, make_position $startpos) }
| IF cond = condition_expr then_block = block ELSE else_block = block
  { If (cond, then_block, Some else_block, make_position $startpos) }
| IF cond = condition_expr then_block = block ELSE IF else_cond = condition_expr else_then_block = block ELSE else_else_block = block
  { If (cond, then_block, Some ([], Some (If (else_cond, else_then_block, Some else_else_block, make_position $startpos))), make_position $startpos) }
| IF cond = condition_expr then_block = block ELSE IF else_cond = condition_expr else_then_block = block
  { If (cond, then_block, Some ([], Some (If (else_cond, else_then_block, None, make_position $startpos))), make_position $startpos) }
| IF cond = condition_expr then_block = block
  { If (cond, then_block, None, make_position $startpos) }
| MATCH expr = simple_expression LBRACE arms = separated_nonempty_list(COMMA, match_arm) RBRACE
  { Match (expr, arms, make_position $startpos) }
| LOOP body = block { Loop (body, make_position $startpos) }
| WHILE cond = condition_expr body = block { While (cond, body, make_position $startpos) }
| FOR var = IDENTIFIER IN iter = condition_expr body = block
  { For (var, iter, body, make_position $startpos) }
| RETURN expr = expression? { Return (expr, make_position $startpos) }
| BREAK expr = expression? { Break (expr, make_position $startpos) }
| CONTINUE { Continue (make_position $startpos) }
| e1 = expression DOTDOT e2 = expression { Range (e1, e2, make_position $startpos) }

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
| MINUS { Neg }
| STAR { Deref }
| BIT_AND { Ref }
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
| name = IDENTIFIER { (name, Identifier (name, make_position $startpos)) }  (* Shorthand syntax *)

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
| let_stmt = located(let_statement) { let (stmt, _pos) = let_stmt in stmt }
| id = IDENTIFIER ASSIGN expr = expression SEMICOLON 
  { AssignStmt (LvalueId id, Assign, expr) }
| id = IDENTIFIER LBRACKET index = expression RBRACKET ASSIGN expr = expression SEMICOLON
  { AssignStmt (LvalueIndex (LvalueId id, index), Assign, expr) }
| id = IDENTIFIER DOT field = IDENTIFIER ASSIGN expr = expression SEMICOLON
  { AssignStmt (LvalueField (LvalueId id, field), Assign, expr) }
| assign_stmt = located(assign_statement) { let (stmt, _pos) = assign_stmt in stmt }
| expr = expression SEMICOLON { ExprStmt expr }
| item = item { ItemStmt item }

(* Helper statement constructors for located pattern *)
let_statement:
| LET is_mut = MUT? name = IDENTIFIER type_ann = type_annotation?
  init = initer? SEMICOLON
  { LetStmt ((is_mut <> None), name, type_ann, init) }

assign_statement:
| lval = lvalue op = assign_op expr = expression SEMICOLON
  { AssignStmt (lval, op, expr) }

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
| for_stmt = for_statement inner = block_inner { let (stmts, expr) = inner in (for_stmt::stmts, expr) }
| for_stmt = for_statement { ([for_stmt], None) }
| while_stmt = while_statement inner = block_inner { let (stmts, expr) = inner in (while_stmt::stmts, expr) }
| while_stmt = while_statement { ([while_stmt], None) }

(* IF statements without else clause - treated as statements *)
if_statement:
| IF cond = condition_expr then_block = block
  { ExprStmt (If (cond, then_block, None, make_position $startpos)) }

(* FOR statements - treated as statements *)
for_statement:
| FOR var = IDENTIFIER IN iter = condition_expr body = block
  { ExprStmt (For (var, iter, body, make_position $startpos)) }

(* WHILE statements - treated as statements *)
while_statement:
| WHILE cond = condition_expr body = block
  { ExprStmt (While (cond, body, make_position $startpos)) }

(* Condition expressions - handles identifiers explicitly to avoid conflicts *)
condition_expr:
| lit = literal { Literal (lit, make_position $startpos) }
| id = IDENTIFIER { Identifier (id, make_position $startpos) }
| LPAREN e = expression RPAREN { e }
| e1 = condition_expr op = binary_op e2 = condition_expr { BinaryOp (e1, op, e2, make_position $startpos(op)) }
| op = unary_op e = condition_expr { UnaryOp (op, e, make_position $startpos) }
| func = condition_expr LPAREN args = separated_list(COMMA, expression) RPAREN
  { FunctionCall (func, args, make_position $startpos) }
| e = condition_expr DOT field = IDENTIFIER { FieldAccess (e, field, make_position $startpos) }
| e1 = condition_expr LBRACKET e2 = expression RBRACKET { Index (e1, e2, make_position $startpos) }
| e1 = condition_expr DOTDOT e2 = condition_expr { Range (e1, e2, make_position $startpos) }
| LBRACKET exprs = separated_list(COMMA, expression) RBRACKET
  { ArrayExpr (exprs, make_position $startpos) }

(* Lists with optional trailing commas *)
field_list:
| (* empty *) { [] }
| field = field_def { [field] }
| field = field_def COMMA { [field] }
| field = field_def COMMA rest = field_list { field :: rest }

variant_list:
| (* empty *) { [] }
| variant = enum_variant { [variant] }
| variant = enum_variant COMMA { [variant] }
| variant = enum_variant COMMA rest = variant_list { variant :: rest }

struct_field_list:
| (* empty *) { [] }
| field = struct_field { [field] }
| field = struct_field COMMA { [field] }
| field = struct_field COMMA rest = struct_field_list { field :: rest }

expression_list:
| (* empty *) { [] }
| expr = expression { [expr] }
| expr = expression COMMA { [expr] }
| expr = expression COMMA rest = expression_list { expr :: rest }

param_list:
| (* empty *) { [] }
| param = param { [param] }
| param = param COMMA { [param] }
| param = param COMMA rest = param_list { param :: rest }

type_list:
| (* empty *) { [] }
| ty = eiron_type { [ty] }
| ty = eiron_type COMMA { [ty] }
| ty = eiron_type COMMA rest = type_list { ty :: rest }

%%
