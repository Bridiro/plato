# Plato Language Extended Specification (Fixed)

## 1. Introduction

Plato is a statically typed, compiled systems programming language with Rust-inspired syntax but C-style semantics. It features strong static typing with type inference, manual memory management, and direct machine code generation without safety guarantees.

## 2. Lexical Structure

### 2.1 Keywords

```
fn, let, mut, if, else, while, for, loop, break, continue, return,
match, struct, enum, impl, trait, use, mod, pub, const, static,
true, false, as, type, in, sizeof, null, usize
```

### 2.2 Identifiers

```ebnf
identifier = (letter | '_') { letter | digit | '_' } ;
letter     = 'a'...'z' | 'A'...'Z' ;
digit      = '0'...'9' ;
```

### 2.3 Literals

```ebnf
integer_literal = decimal_literal | hex_literal | octal_literal | binary_literal ;
decimal_literal = digit { digit } [ integer_suffix ] ;
hex_literal     = '0x' hex_digit { hex_digit } [ integer_suffix ] ;
octal_literal   = '0o' octal_digit { octal_digit } [ integer_suffix ] ;
binary_literal  = '0b' binary_digit { binary_digit } [ integer_suffix ] ;
integer_suffix  = 'i8' | 'i16' | 'i32' | 'i64' | 'u8' | 'u16' | 'u32' | 'u64' | 'usize' ;
hex_digit       = digit | 'a'...'f' | 'A'...'F' ;
octal_digit     = '0'...'7' ;
binary_digit    = '0' | '1' ;

float_literal   = digit { digit } '.' digit { digit } [ float_suffix ] ;
float_suffix    = 'f32' | 'f64' ;

string_literal  = '"' { string_char } '"' ;
string_char     = any_char_except_quote_and_backslash | escape_sequence ;
escape_sequence = '\' ( 'n' | 't' | 'r' | '\' | '"' | '0' ) ;

char_literal    = "'" ( any_char_except_quote_and_backslash | escape_sequence ) "'" ;
bool_literal    = 'true' | 'false' ;
unit_literal    = '(' ')' ;
null_literal    = 'null' ;
```

### 2.4 Operators and Punctuation

```
Arithmetic: + - * / % += -= *= /= %=
Comparison: == != < > <= >=
Logical:    && || !
Bitwise:    & | ^ << >> &= |= ^= <<= >>=
Assignment: =
Pointer:    * & ->
Other:      :: . , ; : ? .. 
Delimiters: ( ) [ ] { }
```

### 2.5 Comments

```ebnf
line_comment  = '//' { any_char_except_newline } newline ;
block_comment = '/*' { any_char | block_comment } '*/' ;
```

## 3. Extended Grammar (EBNF)

```ebnf
(* Program structure *)
program        = { item } ;
item           = function | struct_def | enum_def | trait_def | impl_block
               | global_var | type_alias | use_declaration | mod_declaration ;

(* Declarations *)
global_var     = [ visibility ] [ 'static' | 'const' ] 'let' [ 'mut' ] 
                 identifier [ ':' type ] '=' expression ';' ;
function       = [ visibility ] 'fn' identifier [ generic_params ] 
                 '(' [ param_list ] ')' [ '->' type ] block ;
param_list     = param { ',' param } [ ',' ] ;
param          = identifier ':' type ;

struct_def     = [ visibility ] 'struct' identifier [ generic_params ] 
                 ( struct_body | ';' ) ;
struct_body    = '{' [ field_list ] '}' ;
field_list     = field_def { ',' field_def } [ ',' ] ;
field_def      = [ visibility ] identifier ':' type ;

enum_def       = [ visibility ] 'enum' identifier [ generic_params ] 
                 '{' [ enum_variant_list ] '}' ;
enum_variant_list = enum_variant { ',' enum_variant } [ ',' ] ;
enum_variant   = identifier [ '(' type_list ')' ] [ '=' integer_literal ] ;

trait_def      = [ visibility ] 'trait' identifier [ generic_params ] 
                 '{' { trait_item } '}' ;
trait_item     = function_signature | associated_type ;
function_signature = 'fn' identifier [ generic_params ] '(' [ param_list ] ')' [ '->' type ] ';' ;
associated_type = 'type' identifier [ ':' trait_bounds ] ';' ;

impl_block     = 'impl' [ generic_params ] ( type | trait 'for' type ) 
                 '{' { impl_item } '}' ;
impl_item      = function | type_alias ;

type_alias     = [ visibility ] 'type' identifier [ generic_params ] '=' type ';' ;
use_declaration = [ visibility ] 'use' path ';' ;
mod_declaration = [ visibility ] 'mod' identifier ( ';' | '{' { item } '}' ) ;

(* Types *)
type           = primitive_type | array_type | pointer_type | function_type | path_type ;
primitive_type = 'i8' | 'i16' | 'i32' | 'i64' | 'u8' | 'u16' | 'u32' | 'u64' 
               | 'f32' | 'f64' | 'bool' | 'char' | 'str' | 'void' | 'usize' ;
array_type     = '[' type ';' expression ']' ;
pointer_type   = '*' type ;
function_type  = 'fn' '(' [ type_list ] ')' [ '->' type ] ;
path_type      = path [ '<' type_list '>' ] ;
path           = identifier { '::' identifier } ;

type_list      = type { ',' type } [ ',' ] ;
arg_list       = expression { ',' expression } [ ',' ] ;

(* Generics *)
generic_params = '<' generic_param_list '>' ;
generic_param_list = generic_param { ',' generic_param } [ ',' ] ;
generic_param  = identifier [ ':' trait_bounds ] ;
trait_bounds   = trait_bound { '+' trait_bound } ;
trait_bound    = path_type ;

(* Blocks and Statements *)
block          = '{' { statement } [ expression ] '}' ;
statement      = let_stmt | assign_stmt | expr_stmt | item ;

let_stmt       = 'let' [ 'mut' ] identifier [ ':' type ] [ '=' expression ] ';' ;
assign_stmt    = lvalue assign_op expression ';' ;
assign_op      = '=' | '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=' ;
expr_stmt      = expression ';' ;

lvalue         = identifier | '*' lvalue | lvalue '[' expression ']' | lvalue '.' identifier | lvalue '->' identifier ;

(* Expressions *)
expression     = logical_or_expr ;
logical_or_expr = logical_and_expr { '||' logical_and_expr } ;
logical_and_expr = equality_expr { '&&' equality_expr } ;
equality_expr  = relational_expr { ('==' | '!=') relational_expr } ;
relational_expr = bitwise_or_expr { ('<' | '>' | '<=' | '>=') bitwise_or_expr } ;
bitwise_or_expr = bitwise_xor_expr { '|' bitwise_xor_expr } ;
bitwise_xor_expr = bitwise_and_expr { '^' bitwise_and_expr } ;
bitwise_and_expr = shift_expr { '&' shift_expr } ;
shift_expr     = additive_expr { ('<<' | '>>') additive_expr } ;
additive_expr  = multiplicative_expr { ('+' | '-') multiplicative_expr } ;
multiplicative_expr = cast_expr { ('*' | '/' | '%') cast_expr } ;
cast_expr      = unary_expr [ 'as' type ] ;
unary_expr     = ( '!' | '-' | '*' | '&' | 'sizeof' ) unary_expr | postfix_expr ;
postfix_expr   = primary_expr { postfix_op } ;
postfix_op     = '[' expression ']' | '.' identifier | '->' identifier | '(' [ arg_list ] ')' ;

primary_expr   = literal | identifier | path_expr
               | array_expr | struct_expr | block_expr
               | if_expr | match_expr | loop_expr | while_expr | for_expr
               | return_expr | break_expr | continue_expr
               | '(' expression ')' ;

path_expr      = path ;

(* Control Flow *)
if_expr        = 'if' expression block [ 'else' ( if_expr | block ) ] ;
match_expr     = 'match' expression '{' { match_arm } '}' ;
match_arm      = pattern '=>' ( expression ',' | block ) ;
loop_expr      = 'loop' block ;
while_expr     = 'while' expression block ;
for_expr       = 'for' identifier 'in' expression block ;
return_expr    = 'return' [ expression ] ;
break_expr     = 'break' [ expression ] ;
continue_expr  = 'continue' ;

(* Patterns *)
pattern        = literal_pattern | identifier_pattern | wildcard_pattern
               | enum_pattern | tuple_pattern ;
literal_pattern = literal ;
identifier_pattern = identifier ;
wildcard_pattern = '_' ;
enum_pattern   = path [ '(' pattern_list ')' ] ;
tuple_pattern  = '(' [ pattern_list ] ')' ;
pattern_list   = pattern { ',' pattern } [ ',' ] ;

(* Literals and expressions *)
array_expr     = '[' [ expression_list ] ']' ;
expression_list = expression { ',' expression } [ ',' ] ;
struct_expr    = path '{' [ field_init_list ] '}' ;
field_init_list = field_init { ',' field_init } [ ',' ] ;
field_init     = identifier ':' expression ;
block_expr     = block ;

literal        = integer_literal | float_literal | string_literal 
               | char_literal | bool_literal | unit_literal | null_literal ;

visibility     = 'pub' ;
```

## 4. Type System

### 4.1 Primitive Types

```
Signed integers:   i8, i16, i32, i64
Unsigned integers: u8, u16, u32, u64, usize
Floating point:    f32, f64
Boolean:           bool
Character:         char (ASCII)
String:            str (null-terminated)
Void:              void (for functions with no return)
```

### 4.2 Compound Types

```
Arrays:     [T; N]           (fixed size, stack allocated)
Pointers:   *T               (raw pointers, no safety)
Functions:  fn(T1, T2) -> T3
Structs:    User-defined named types
Enums:      Sum types with optional data
```

### 4.3 Typing Rules

#### Environment
```
Γ ::= ∅ | Γ, x: τ | Γ, T: Kind
```

#### Literals
```
————————————————
Γ ⊢ n: i32  (default integer)

————————————————
Γ ⊢ n_suffix: suffix_type

————————————————
Γ ⊢ f: f64  (default float)

————————————————
Γ ⊢ f_suffix: suffix_type

————————————————
Γ ⊢ true: bool

————————————————
Γ ⊢ false: bool

————————————————
Γ ⊢ 'c': char

————————————————
Γ ⊢ "str": *char

————————————————
Γ ⊢ (): void

————————————————
Γ ⊢ null: *T  (for any T)
```

#### Variables
```
x: τ ∈ Γ
————————————————
Γ ⊢ x: τ
```

#### Pointer Operations
```
Γ ⊢ e: τ
————————————————
Γ ⊢ &e: *τ

Γ ⊢ e: *τ
————————————————
Γ ⊢ *e: τ

Γ ⊢ e: *struct S    field f: τ ∈ S
————————————————————————————————————
Γ ⊢ e->f: τ

Γ ⊢ e: struct S    field f: τ ∈ S
————————————————————————————————————
Γ ⊢ e.f: τ
```

#### Arrays
```
Γ ⊢ e1: [τ; n]    Γ ⊢ e2: integer_type
————————————————————————————————————————
Γ ⊢ e1[e2]: τ

Γ ⊢ e1: *τ    Γ ⊢ e2: integer_type
————————————————————————————————————
Γ ⊢ e1[e2]: τ
```

#### Binary Operations
```
Γ ⊢ e1: τ    Γ ⊢ e2: τ    τ ∈ numeric_types
————————————————————————————————————————————
Γ ⊢ e1 + e2: τ

Γ ⊢ e1: *τ    Γ ⊢ e2: integer_type
————————————————————————————————————
Γ ⊢ e1 + e2: *τ

Γ ⊢ e1: τ    Γ ⊢ e2: τ    τ supports equality
————————————————————————————————————————————
Γ ⊢ e1 == e2: bool

Γ ⊢ e1: bool    Γ ⊢ e2: bool
——————————————————————————————
Γ ⊢ e1 && e2: bool
```

#### Type Casting
```
Γ ⊢ e: τ1    τ1 can_cast_to τ2
————————————————————————————————
Γ ⊢ e as τ2: τ2
```

#### Function Types
```
Γ, x1: τ1, ..., xn: τn ⊢ body: τ
——————————————————————————————————————————————————
Γ ⊢ fn f(x1: τ1, ..., xn: τn) -> τ { body }: fn(τ1, ..., τn) -> τ

Γ ⊢ f: fn(τ1, ..., τn) -> τ    Γ ⊢ e1: τ1    ...    Γ ⊢ en: τn
————————————————————————————————————————————————————————————————
Γ ⊢ f(e1, ..., en): τ
```

#### Let Bindings
```
Γ ⊢ e: τ
———————————————————————————————————————
Γ ⊢ let x = e: (Γ[x ↦ τ], void)

Γ ⊢ e: τ    τ <: τ'
————————————————————————
Γ ⊢ let x: τ' = e: (Γ[x ↦ τ'], void)
```

#### Control Flow
```
Γ ⊢ cond: bool    Γ ⊢ e1: τ    Γ ⊢ e2: τ
——————————————————————————————————————————
Γ ⊢ if cond { e1 } else { e2 }: τ

Γ ⊢ cond: bool    Γ ⊢ body: void
————————————————————————————————————
Γ ⊢ while cond { body }: void

Γ ⊢ iter: [τ; n]    Γ, x: τ ⊢ body: void
———————————————————————————————————————————
Γ ⊢ for x in iter { body }: void

Γ ⊢ start: τ    Γ ⊢ end: τ    τ ∈ integer_types    Γ, x: τ ⊢ body: void
————————————————————————————————————————————————————————————————————————
Γ ⊢ for x in start..end { body }: void
```

#### Pattern Matching
```
Γ ⊢ e: τ    Γ ⊢ p1: τ → Γ1    Γ1 ⊢ e1: τ'    ...    Γ ⊢ pn: τ → Γn    Γn ⊢ en: τ'
————————————————————————————————————————————————————————————————————————————————————
Γ ⊢ match e { p1 => e1, ..., pn => en }: τ'
```

### 4.4 Type Inference Algorithm

1. **Constraint Generation**: Walk AST and generate type constraints
2. **Unification**: Solve constraints using unification algorithm
3. **Substitution**: Apply unified types back to AST

```
Unify(τ1, τ2):
  case (τ1, τ2):
    (T, T) → success
    (T, _) → [T ↦ τ2] if T is type variable
    (_, T) → [T ↦ τ1] if T is type variable
    (F(τ1...τn), F(σ1...σn)) → Unify(τ1, σ1) ∪ ... ∪ Unify(τn, σn)
    _ → failure
```

## 5. Memory Model

### 5.1 Memory Layout

- **Stack**: Local variables, function parameters, return addresses
- **Heap**: Dynamic allocations via `malloc`/`free` style functions
- **Global**: Static and global variables
- **Code**: Function definitions and constants

### 5.2 Pointer Semantics

```
*T    - Raw pointer to T (can be null, no bounds checking)
```

All pointer operations are unsafe by default:
- Dereferencing null pointers causes undefined behavior
- Buffer overflows are not prevented
- Use-after-free is possible
- Double-free is possible

### 5.3 Memory Management

Manual memory management with built-in functions:

```rust
fn malloc(size: usize) -> *void;
fn free(ptr: *void);
fn realloc(ptr: *void, size: usize) -> *void;
```

## 6. Operational Semantics

### 6.1 Values and Memory

```
v ::= n | f | true | false | 'c' | "str" | null | addr
    | [v1, ..., vn] | {f1: v1, ..., fn: vn} | Variant(v1, ..., vn)

Store ::= { addr ↦ v }
Env ::= { x ↦ addr }
```

### 6.2 Evaluation Rules

Configuration: ⟨e, ρ, σ⟩ where e is expression, ρ is environment, σ is store

#### Basic Values
```
—————————————————————————
⟨n, ρ, σ⟩ ⇓ ⟨n, ρ, σ⟩

x ∈ dom(ρ)    ρ(x) = addr    σ(addr) = v
——————————————————————————————————————
⟨x, ρ, σ⟩ ⇓ ⟨v, ρ, σ⟩
```

#### Pointer Operations
```
⟨e, ρ, σ⟩ ⇓ ⟨addr, ρ, σ'⟩    σ'(addr) = v
——————————————————————————————————————
⟨*e, ρ, σ⟩ ⇓ ⟨v, ρ, σ'⟩

⟨e, ρ, σ⟩ ⇓ ⟨v, ρ, σ'⟩    addr = fresh()
————————————————————————————————————————
⟨&e, ρ, σ⟩ ⇓ ⟨addr, ρ, σ'[addr ↦ v]⟩
```

#### Binary Operations
```
⟨e1, ρ, σ⟩ ⇓ ⟨v1, ρ, σ1⟩    ⟨e2, ρ, σ1⟩ ⇓ ⟨v2, ρ, σ2⟩    v3 = v1 ⊕ v2
————————————————————————————————————————————————————————————————————————
⟨e1 ⊕ e2, ρ, σ⟩ ⇓ ⟨v3, ρ, σ2⟩
```

#### Assignment
```
⟨lval, ρ, σ⟩ ⇓ ⟨addr, ρ, σ'⟩    ⟨e, ρ, σ'⟩ ⇓ ⟨v, ρ, σ''⟩
————————————————————————————————————————————————————————————
⟨lval = e, ρ, σ⟩ ⇓ ⟨v, ρ, σ''[addr ↦ v]⟩
```

#### Control Flow
```
⟨cond, ρ, σ⟩ ⇓ ⟨true, ρ, σ'⟩    ⟨e1, ρ, σ'⟩ ⇓ ⟨v, ρ, σ''⟩
————————————————————————————————————————————————————————————
⟨if cond { e1 } else { e2 }, ρ, σ⟩ ⇓ ⟨v, ρ, σ''⟩

⟨cond, ρ, σ⟩ ⇓ ⟨false, ρ, σ'⟩    ⟨e2, ρ, σ'⟩ ⇓ ⟨v, ρ, σ''⟩
—————————————————————————————————————————————————————————————
⟨if cond { e1 } else { e2 }, ρ, σ⟩ ⇓ ⟨v, ρ, σ''⟩
```

#### Loops
```
⟨cond, ρ, σ⟩ ⇓ ⟨false, ρ, σ'⟩
————————————————————————————————————
⟨while cond { body }, ρ, σ⟩ ⇓ ⟨(), ρ, σ'⟩

⟨cond, ρ, σ⟩ ⇓ ⟨true, ρ, σ'⟩    ⟨body, ρ, σ'⟩ ⇓ ⟨(), ρ, σ''⟩    ⟨while cond { body }, ρ, σ''⟩ ⇓ ⟨(), ρ, σ'''⟩
—————————————————————————————————————————————————————————————————————————————————————————————————————————————
⟨while cond { body }, ρ, σ⟩ ⇓ ⟨(), ρ, σ'''⟩
```

## 7. Standard Library

### 7.1 Memory Management

```rust
fn malloc(size: usize) -> *void;
fn free(ptr: *void);
fn realloc(ptr: *void, size: usize) -> *void;
fn memcpy(dest: *void, src: *void, n: usize) -> *void;
fn memset(ptr: *void, value: i32, n: usize) -> *void;
```

### 7.2 String Operations

```rust
fn strlen(s: *char) -> usize;
fn strcpy(dest: *char, src: *char) -> *char;
fn strcmp(s1: *char, s2: *char) -> i32;
fn strcat(dest: *char, src: *char) -> *char;
```

### 7.3 I/O Functions

```rust
fn printf(format: *char, ...) -> i32;
fn scanf(format: *char, ...) -> i32;
fn puts(s: *char) -> i32;
fn getchar() -> i32;
fn putchar(c: i32) -> i32;
```

### 7.4 Math Functions

```rust
fn abs(x: i32) -> i32;
fn sqrt(x: f64) -> f64;
fn pow(base: f64, exp: f64) -> f64;
fn sin(x: f64) -> f64;
fn cos(x: f64) -> f64;
```

## 8. Example Programs

### 8.1 Hello World

```rust
fn main() -> i32 {
    printf("Hello, World!\n");
    0
}
```

### 8.2 Manual Memory Management

```rust
fn main() -> i32 {
    let ptr = malloc(sizeof(i32) * 10) as *i32;
    if ptr == null {
        return 1;
    }
    
    // Use the memory
    for i in 0..10 {
        *(ptr + i) = i * i;
    }
    
    // Print values
    for i in 0..10 {
        printf("%d ", *(ptr + i));
    }
    
    free(ptr as *void);
    0
}
```

### 8.3 Linked List

```rust
struct Node {
    data: i32,
    next: *Node,
}

fn create_node(data: i32) -> *Node {
    let node = malloc(sizeof(Node)) as *Node;
    if node != null {
        node->data = data;
        node->next = null;
    }
    node
}

fn append(head: *Node, data: i32) -> *Node {
    let new_node = create_node(data);
    if head == null {
        return new_node;
    }
    
    let current = head;
    while current->next != null {
        current = current->next;
    }
    current->next = new_node;
    head
}

fn print_list(head: *Node) {
    let current = head;
    while current != null {
        printf("%d -> ", current->data);
        current = current->next;
    }
    printf("null\n");
}

fn free_list(head: *Node) {
    while head != null {
        let temp = head;
        head = head->next;
        free(temp as *void);
    }
}
```

### 8.4 Generic Function

```rust
fn max<T>(a: T, b: T) -> T {
    if a > b { a } else { b }
}

fn main() -> i32 {
    let x = max(10, 20);
    let y = max(3.14, 2.71);
    printf("max(10, 20) = %d\n", x);
    printf("max(3.14, 2.71) = %f\n", y);
    0
}
```

### 8.5 Pattern Matching

```rust
enum Color {
    Red,
    Green,
    Blue,
    RGB(u8, u8, u8),
}

fn describe_color(c: Color) -> *char {
    match c {
        Color::Red => "Red like fire",
        Color::Green => "Green like grass",
        Color::Blue => "Blue like sky",
        Color::RGB(r, g, b) => {
            printf("RGB(%d, %d, %d)", r, g, b);
            "Custom color"
        },
    }
}
```

## 9. Compilation Model

### 9.1 Compilation Phases

1. **Lexical Analysis**: Source → Tokens
2. **Parsing**: Tokens → AST
3. **Name Resolution**: Resolve identifiers and paths
4. **Type Checking**: Verify type correctness and infer types
5. **Monomorphization**: Generate concrete versions of generic functions
6. **Code Generation**: AST → Assembly/Machine Code
7. **Linking**: Combine object files and libraries

### 9.2 Memory Layout

```
High Memory
+------------------+
|      Stack       |  (grows down)
+------------------+
|        ↓         |
|                  |
|        ↑         |
+------------------+
|      Heap        |  (grows up)
+------------------+
|   Global Data    |
+------------------+
|   Code Segment   |
+------------------+
Low Memory
```

### 9.3 ABI Compatibility

Plato uses C-compatible ABI for external function calls:
- Functions follow platform calling conventions
- Structs have C-compatible layout
- Pointers are raw machine addresses
- No name mangling for `extern "C"` functions
