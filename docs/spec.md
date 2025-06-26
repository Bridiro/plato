# Plato Language Specification

## 1. Introduction

Plato is a minimal, statically typed, compiled programming language. It is designed to be fast, simple, and expressive. Plato prioritizes clean syntax, strong static typing, and direct machine code generation. This document defines its syntax and semantics formally.

## 2. Lexical Structure

### 2.1 Keywords

```
fn, let, if, else, return, true, false
```

### 2.2 Identifiers

Identifiers begin with a letter (a-z, A-Z) and may contain letters, digits, and underscores.

```
identifier = letter { letter | digit | '_' };
```

### 2.3 Literals

* **Integer**: Decimal whole numbers (e.g., `42`)
* **Float**: Numbers with a decimal point (e.g., `3.14`)
* **Boolean**: `true`, `false`
* **Character**: Single characters in single quotes (e.g., `'a'`)
* **Unit**: `()`

### 2.4 Comments

Single-line comments start with `//` and continue to the end of the line.

### 2.5 Whitespace

Whitespace separates tokens but is otherwise ignored. Line breaks are insignificant.

## 3. Syntax (EBNF Grammar)

```ebnf
(* Lexical elements *)
letter     = "a" | ... | "z" | "A" | ... | "Z" ;
digit      = "0" | ... | "9" ;
identifier = letter { letter | digit | '_' } ;
integer    = digit { digit } ;
float      = digit { digit } "." digit { digit } ;
boolean    = "true" | "false" ;
char       = "'" any_char "'" ;
unit       = "()" ;

(* Program structure *)
program        = { top_level_item } ;
top_level_item = global_var_decl | function ;

(* Declarations *)
global_var_decl = "let" identifier [ ":" type ] "=" expression ";" ;
function        = "fn" identifier "(" [ param_list ] ")" "->" type block ;
param_list      = param { "," param } ;
param           = identifier ":" type ;

(* Blocks and statements *)
block       = "{" { statement } [ expression ] "}" ;
statement   = variable_decl
            | assignment
            | return_stmt
            | expr_stmt
            | if_expr ;

variable_decl = "let" identifier [ ":" type ] "=" expression ";" ;
assignment    = identifier "=" expression ";" ;
return_stmt   = "return" expression ";" ;
expr_stmt     = expression ";" ;

(* Expressions *)
expression     = if_expr | comparison ;
if_expr        = "if" expression block [ "else" block ] ;
comparison     = addition { comp_op addition } ;
comp_op        = "==" | "!=" | "<" | ">" | "<=" | ">=" ;
addition       = multiplication { add_op multiplication } ;
add_op         = "+" | "-" ;
multiplication = unary { mul_op unary } ;
mul_op         = "*" | "/" ;
unary          = ( "!" | "-" ) unary | primary ;

primary = literal
        | identifier
        | function_call
        | "(" expression ")" ;

function_call = identifier "(" [ argument_list ] ")" ;
argument_list = expression { "," expression } ;

(* Types *)
type = "int" | "float" | "bool" | "char" | "unit" ;
```

## 4. Static Semantics (Typing Rules)

### 4.1 Types

Plato supports the following primitive types:

```
int, float, bool, char, unit
```

### 4.2 Type Environments

Typing judgments are made in the context of an environment `Γ`, which maps identifiers to types:

```
Γ ⊢ e : τ
```

means "under environment Γ, expression `e` has type `τ`."

### 4.3 Literals

```
Γ ⊢ 42 : int
Γ ⊢ 3.14 : float
Γ ⊢ true : bool
Γ ⊢ false : bool
Γ ⊢ 'a' : char
Γ ⊢ () : unit
```

### 4.4 Variables

```
Γ(x) = τ
———————
Γ ⊢ x : τ
```

### 4.5 Binary Operations

Arithmetic (e.g., `+`, `-`, `*`, `/`) and comparisons:

```
Γ ⊢ e1 : int   Γ ⊢ e2 : int
———————————————
Γ ⊢ e1 + e2 : int

Γ ⊢ e1 : float   Γ ⊢ e2 : float
———————————————
Γ ⊢ e1 + e2 : float

Γ ⊢ e1 : τ   Γ ⊢ e2 : τ   τ ∈ { int, float, bool, char }
———————————————————————————————
Γ ⊢ e1 == e2 : bool
```

### 4.6 Let Binding

```
Γ ⊢ e : τ
——————————————
Γ ⊢ let x = e : Γ[x ↦ τ]

Γ ⊢ e : τ     τ = τ'
——————————————
Γ ⊢ let x: τ' = e : Γ[x ↦ τ']
```

### 4.7 If Expression

```
Γ ⊢ cond : bool   Γ ⊢ e1 : τ   Γ ⊢ e2 : τ
——————————————————————————————
Γ ⊢ if cond { e1 } else { e2 } : τ
```

### 4.8 Function Definition and Call

Let `f(x: τ1, y: τ2) -> τ` be defined as:

```
Γ[x ↦ τ1, y ↦ τ2] ⊢ body : τ
———————————————————————————
Γ ⊢ fn f(x: τ1, y: τ2) -> τ { body } : f : τ1 × τ2 → τ
```

Function application:

```
Γ ⊢ f : τ1 × τ2 → τ   Γ ⊢ e1 : τ1   Γ ⊢ e2 : τ2
——————————————————————————————————————————————
Γ ⊢ f(e1, e2) : τ
```

### 4.9 Return Statement

```
Γ ⊢ e : τ
———————————
Γ ⊢ return e : τ (terminates block with value)
```

### 4.10 Block

```
Γ ⊢ s1 : Γ1   Γ1 ⊢ s2 : Γ2   ...   Γn ⊢ e : τ
———————————————————————————————
Γ ⊢ { s1; s2; ...; e } : τ
```

## 5. Execution Semantics

The operational semantics of Plato is defined using a small-step evaluation relation:

```
e ⇓ v
```

meaning "expression `e` evaluates to value `v`."

### 5.1 Values

```
v ::= n | f | true | false | 'a' | ()
```

Where `n` is an integer or float literal and `f` is a function closure.

### 5.2 Environment

Evaluation uses an environment `σ`, mapping variables to runtime values.

### 5.3 Variable Lookup

```
σ(x) = v
————————
σ ⊢ x ⇓ v
```

### 5.4 Binary Operations

```
σ ⊢ e1 ⇓ n1   σ ⊢ e2 ⇓ n2   n3 = n1 + n2
——————————————————————————————
σ ⊢ e1 + e2 ⇓ n3
```

(Similar rules for `-`, `*`, `/`, `==`, `!=`, etc.)

### 5.5 Let Binding

```
σ ⊢ e ⇓ v
——————————————
σ ⊢ let x = e ⇓ σ[x ↦ v]
```

### 5.6 If Expression

```
σ ⊢ cond ⇓ true    σ ⊢ e1 ⇓ v
——————————————————————
σ ⊢ if cond { e1 } else { e2 } ⇓ v

σ ⊢ cond ⇓ false   σ ⊢ e2 ⇓ v
——————————————————————
σ ⊢ if cond { e1 } else { e2 } ⇓ v
```

### 5.7 Function Application

```
σ ⊢ e1 ⇓ v1   ...   σ ⊢ en ⇓ vn
f(x1, ..., xn) = body   σ' = σ[x1 ↦ v1, ..., xn ↦ vn]
σ' ⊢ body ⇓ v
——————————————————————————
σ ⊢ f(e1, ..., en) ⇓ v
```

### 5.8 Block Evaluation

```
σ ⊢ s1 ⇓ σ1   σ1 ⊢ s2 ⇓ σ2   ...   σn ⊢ e ⇓ v
————————————————————————————————————
σ ⊢ { s1; s2; ...; e } ⇓ v
```

## 6. Abstract Syntax Tree (AST)

### 6.1 Positions and Spans

```ocaml
type position = { line: int; column: int }
type span = { start_pos: position; end_pos: position }
type 'a located = { node: 'a; span: span }
```

### 6.2 Program

```ocaml
type program = toplevel_item list
and toplevel_item =
  | GlobalLet of string * typ option * expr
  | Function of string * (string * typ) list * typ * block
```

### 6.3 Blocks and Statements

```ocaml
type block = statement list * expr option

and statement = statement_node located
and statement_node =
  | Let of string * typ option * expr
  | Assign of string * expr
  | Return of expr
  | ExprStmt of expr
  | If of expr * block * block option
```

### 6.4 Expressions

```ocaml
type expr = expr_node located

and expr_node =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | UnitLit
  | Var of string
  | Unary of unary_op * expr
  | Binary of binary_op * expr * expr
  | Call of string * expr list
  | Grouped of expr

and unary_op = Not | Neg

and binary_op =
  | Add | Sub | Mul | Div
  | Eq | Neq | Lt | Gt | Le | Ge
```

### 6.5 Types

```ocaml
type typ = typ_node located

and typ_node =
  | TInt
  | TFloat
  | TBool
  | TChar
  | TUnit
```

## 7. Built-in Functions (Planned)

* `println(...)` — basic output
* `assert(...)` — debugging utility

## 8. Future Extensions

* string literals
* Structs and enums
* Modules and namespaces
