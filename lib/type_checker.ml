open Ast
open Error

(* Use Error module's position type *)
type position = Error.position

(* Helper function to convert AST position to Error position *)
let ast_pos_to_error_pos (ast_pos : Ast.position) : Error.position =
  { line = ast_pos.line; column = ast_pos.column; offset = 0 }

(* Helper function to convert Error position to AST position *)
let error_pos_to_ast_pos (error_pos : Error.position) : Ast.position =
  { line = error_pos.line; column = error_pos.column }

(* Helper function to extract position from expressions *)
let get_expression_position = function
  | Literal (_, pos) -> pos
  | Identifier (_, pos) -> pos
  | BinaryOp (_, _, _, pos) -> pos
  | UnaryOp (_, _, pos) -> pos
  | FunctionCall (_, _, pos) -> pos
  | FieldAccess (_, _, pos) -> pos
  | Cast (_, _, pos) -> pos
  | Index (_, _, pos) -> pos
  | StructExpr (_, _, pos) -> pos
  | PathExpr (_, pos) -> pos
  | _ -> { Ast.line = 0; column = 0 }  (* Fallback for expressions without position *)

(* Comprehensive type system matching AST types *)
type simple_type =
  | TI8
  | TI16
  | TI32
  | TI64
  | TU8
  | TU16
  | TU32
  | TU64
  | TUsize
  | TF32
  | TF64
  | TBool
  | TChar
  | TStr
  | TVoid
  | TUnit
  | TArray of simple_type * int option
  | TPointer of simple_type
  | TFunction of simple_type list * simple_type
  | TStruct of string * (string * simple_type) list
  | TEnum of string * string list
  | TGeneric of string
  | TNullPtr

(* Enhanced Symbol Table with Hierarchical Scoping *)
module SymbolTable = struct
  type symbol_info = {
    name: string;
    symbol_type: simple_type;
    scope_level: int;
    is_mutable: bool;
    location: position;
    is_parameter: bool;
    is_used: bool ref;
  }

  type scope = {
    symbols: (string, symbol_info) Hashtbl.t;
    parent: scope option;
    level: int;
    scope_kind: scope_kind;
  }

  and scope_kind =
    | GlobalScope
    | FunctionScope of string
    | BlockScope
    | LoopScope
    | IfScope

  type t = {
    mutable current_scope: scope;
    mutable function_context: string option;
    mutable filename: string option;
    (* Global registries *)
    functions: (string, simple_type list * simple_type * Error.position) Hashtbl.t;
    structs: (string, (string * simple_type) list * Error.position) Hashtbl.t;
    enums: (string, string list * Error.position) Hashtbl.t;
    traits: (string, string list * Error.position) Hashtbl.t;
  }

  let create_scope parent level scope_kind =
    { symbols = Hashtbl.create 16; parent; level; scope_kind }

  let create () =
    let global_scope = create_scope None 0 GlobalScope in
    {
      current_scope = global_scope;
      function_context = None;
      filename = None;
      functions = Hashtbl.create 32;
      structs = Hashtbl.create 16;
      enums = Hashtbl.create 16;
      traits = Hashtbl.create 16;
    }

  let set_filename table filename =
    table.filename <- Some filename

  let push_scope table scope_kind =
    let new_level = table.current_scope.level + 1 in
    let new_scope = create_scope (Some table.current_scope) new_level scope_kind in
    table.current_scope <- new_scope

  let pop_scope table =
    match table.current_scope.parent with
    | Some parent -> table.current_scope <- parent
    | None -> failwith "Cannot pop global scope"

  let enter_function table func_name =
    table.function_context <- Some func_name;
    push_scope table (FunctionScope func_name)

  let exit_function table =
    table.function_context <- None;
    pop_scope table

  (* Symbol declaration with shadowing detection *)
  let declare_symbol table name symbol_type is_mutable pos is_parameter =
    let symbol = {
      name;
      symbol_type;
      scope_level = table.current_scope.level;
      is_mutable;
      location = pos;
      is_parameter;
      is_used = ref false;
    } in
    
    (* Check for shadowing in the same scope *)
    if Hashtbl.mem table.current_scope.symbols name then
      let existing = Hashtbl.find table.current_scope.symbols name in
      let error_msg = Printf.sprintf "Variable '%s' is already declared in this scope at line %d" 
                      name existing.location.line in
      let span = Error.make_span pos pos table.filename in
      let error = Error.make_error (Error.SemanticError error_msg) span error_msg in
      raise (Error.CompilerError error)
    else
      Hashtbl.replace table.current_scope.symbols name symbol

  (* Symbol lookup with proper scoping *)
  let lookup_symbol table name =
    let rec search_scope scope =
      match Hashtbl.find_opt scope.symbols name with
      | Some symbol ->
          symbol.is_used := true;  (* Mark as used *)
          Some symbol
      | None ->
          match scope.parent with
          | Some parent -> search_scope parent
          | None -> None
    in
    search_scope table.current_scope

  (* Type-specific registration functions *)
  let register_function table name param_types return_type pos =
    if Hashtbl.mem table.functions name then
      let error_msg = Printf.sprintf "Function '%s' is already defined" name in
      let span = Error.make_span pos pos table.filename in
      let error = Error.make_error (Error.SemanticError error_msg) span error_msg in
      raise (Error.CompilerError error)
    else
      Hashtbl.replace table.functions name (param_types, return_type, pos)

  let lookup_function table name =
    Hashtbl.find_opt table.functions name

  let register_struct table name fields pos =
    if Hashtbl.mem table.structs name then
      let error_msg = Printf.sprintf "Struct '%s' is already defined" name in
      let span = Error.make_span pos pos table.filename in
      let error = Error.make_error (Error.SemanticError error_msg) span error_msg in
      raise (Error.CompilerError error)
    else
      Hashtbl.replace table.structs name (fields, pos)

  let lookup_struct table name =
    Hashtbl.find_opt table.structs name

  let register_enum table name variants pos =
    if Hashtbl.mem table.enums name then
      let error_msg = Printf.sprintf "Enum '%s' is already defined" name in
      let span = Error.make_span pos pos table.filename in
      let error = Error.make_error (Error.SemanticError error_msg) span error_msg in
      raise (Error.CompilerError error)
    else
      Hashtbl.replace table.enums name (variants, pos)

  let lookup_enum table name =
    Hashtbl.find_opt table.enums name

  (* Analysis functions *)
  let check_unused_variables table =
    let rec check_scope scope warnings =
      let scope_warnings = Hashtbl.fold (fun name symbol acc ->
        if not !(symbol.is_used) && not symbol.is_parameter && symbol.scope_level > 0 then
          let warning_msg = Printf.sprintf "Unused variable '%s'" name in
          let span = Error.make_span symbol.location symbol.location table.filename in
          let warning = Error.make_error (Error.SemanticError warning_msg) span warning_msg in
          warning :: acc
        else acc
      ) scope.symbols [] in
      
      let parent_warnings = match scope.parent with
        | Some parent -> check_scope parent warnings
        | None -> warnings
      in
      scope_warnings @ parent_warnings
    in
    check_scope table.current_scope []

  let get_current_scope_level table =
    table.current_scope.level

  let is_in_function table =
    table.function_context <> None

  let get_function_context table =
    table.function_context
end

(* Error reporting with enhanced symbol table integration *)
let type_error table message (pos : Ast.position) =
  let error_pos = ast_pos_to_error_pos pos in
  let span = Error.make_span error_pos error_pos table.SymbolTable.filename in
  let error = Error.make_error (Error.TypeError message) span message in
  raise (Error.CompilerError error)

let semantic_error table message (pos : Ast.position) =
  let error_pos = ast_pos_to_error_pos pos in
  let span = Error.make_span error_pos error_pos table.SymbolTable.filename in
  let error = Error.make_error (Error.SemanticError message) span message in
  raise (Error.CompilerError error)

(* Convert AST literal to simple type *)
let type_of_literal = function
  | IntLit (_, suffix_opt) ->
    (match suffix_opt with
    | Some I8 -> TI8
    | Some I16 -> TI16
    | Some I32 -> TI32
    | Some I64 -> TI64
    | Some U8 -> TU8
    | Some U16 -> TU16
    | Some U32 -> TU32
    | Some U64 -> TU64
    | Some Usize -> TUsize
    | None -> TI32)
  | FloatLit (_, suffix_opt) ->
    (match suffix_opt with
    | Some F32 -> TF32
    | Some F64 -> TF64
    | None -> TF64)
  | StringLit _ -> TStr
  | CharLit _ -> TChar
  | BoolLit _ -> TBool
  | UnitLit -> TUnit
  | NullLit -> TNullPtr

(* Convert AST types to simple types *)
let rec ast_type_to_simple_type = function
  | PrimType prim ->
    (match prim with
    | I8 -> TI8
    | I16 -> TI16
    | I32 -> TI32
    | I64 -> TI64
    | U8 -> TU8
    | U16 -> TU16
    | U32 -> TU32
    | U64 -> TU64
    | Usize -> TUsize
    | F32 -> TF32
    | F64 -> TF64
    | Bool -> TBool
    | Char -> TChar
    | Str -> TStr
    | Void -> TVoid)
  | PathType (path, _) ->
    let type_name = String.concat "::" path in
    (match path with
    | ["int"] -> TI32  (* Default int to i32 *)
    | ["float"] -> TF64  (* Default float to f64 *)
    | ["string"] -> TStr
    | _ -> TStruct (type_name, []))  (* Placeholder - will be resolved later *)
  | PointerType inner_type ->
    TPointer (ast_type_to_simple_type inner_type)
  | ArrayType (element_type, size_expr) ->
    (* For now, we'll handle size as None for dynamic arrays *)
    TArray (ast_type_to_simple_type element_type, None)
  | FunctionType (param_types, return_type_opt) ->
    let param_simple_types = List.map ast_type_to_simple_type param_types in
    let return_simple_type = match return_type_opt with
      | Some return_type -> ast_type_to_simple_type return_type
      | None -> TUnit
    in
    TFunction (param_simple_types, return_simple_type)
  | GenericType name ->
    TGeneric name
  | SelfType _ ->
    (* Self type will be resolved based on context *)
    TGeneric "Self"

(* Helper functions for type checking *)
let types_compatible expected actual =
  expected = actual || 
  (match expected, actual with
  | TPointer _, TNullPtr -> true
  | TI32, TI8 | TI32, TI16 -> true  (* Allow implicit numeric promotion *)
  | TI64, TI32 | TI64, TI16 | TI64, TI8 -> true
  | _ -> false)

let is_numeric_type = function
  | TI8 | TI16 | TI32 | TI64 | TU8 | TU16 | TU32 | TU64 | TUsize | TF32 | TF64 -> true
  | _ -> false

let is_integer_type = function
  | TI8 | TI16 | TI32 | TI64 | TU8 | TU16 | TU32 | TU64 | TUsize -> true
  | _ -> false

let rec string_of_simple_type = function
  | TI8 -> "i8" | TI16 -> "i16" | TI32 -> "i32" | TI64 -> "i64"
  | TU8 -> "u8" | TU16 -> "u16" | TU32 -> "u32" | TU64 -> "u64"
  | TUsize -> "usize" | TF32 -> "f32" | TF64 -> "f64"
  | TBool -> "bool" | TChar -> "char" | TStr -> "str" | TVoid -> "void" | TUnit -> "()"
  | TArray (elem_type, Some size) -> Printf.sprintf "[%s; %d]" (string_of_simple_type elem_type) size
  | TArray (elem_type, None) -> Printf.sprintf "[%s]" (string_of_simple_type elem_type)
  | TPointer inner_type -> Printf.sprintf "*%s" (string_of_simple_type inner_type)
  | TFunction (param_types, return_type) ->
    let params = String.concat ", " (List.map string_of_simple_type param_types) in
    Printf.sprintf "fn(%s) -> %s" params (string_of_simple_type return_type)
  | TStruct (name, _) -> name
  | TEnum (name, _) -> name
  | TGeneric name -> name
  | TNullPtr -> "null"

(* Type checking with enhanced symbol table *)
let initialize_builtin_functions table =
  let builtins = [
    ("print", [TStr], TUnit);
    ("println", [TStr], TUnit);
    ("int_to_string", [TI32], TStr);
    ("string_to_int", [TStr], TI32);
  ] in
  List.iter (fun (name, params, ret) ->
    let pos = { Error.line = 0; column = 0; offset = 0 } in
    SymbolTable.register_function table name params ret pos
  ) builtins

(* Enhanced expression type checking with proper scoping *)
let rec check_expression table = function
  | Literal (lit, pos) -> type_of_literal lit
  | Identifier (name, pos) ->
    (match SymbolTable.lookup_symbol table name with
    | Some symbol -> symbol.symbol_type
    | None ->
      let error_msg = Printf.sprintf "Undefined variable '%s'" name in
      type_error table error_msg pos)
  | BinaryOp (left, op, right, pos) ->
    let left_type = check_expression table left in
    let right_type = check_expression table right in
    (* Type compatibility checking *)
    (match op with
    | Eq | Ne | Lt | Gt | Le | Ge ->
      if left_type = right_type then TBool
      else
        let error_msg = Printf.sprintf "Cannot compare values of different types: expected `%s`, found `%s`" 
          (string_of_simple_type left_type) (string_of_simple_type right_type) in
        type_error table error_msg pos
    | Add | Sub | Mul | Div | Mod ->
      if is_numeric_type left_type && is_numeric_type right_type then
        if left_type = right_type then left_type
        else
          let error_msg = Printf.sprintf "Type mismatch in binary operation: cannot apply operator to `%s` and `%s`"
            (string_of_simple_type left_type) (string_of_simple_type right_type) in
          type_error table error_msg pos
      else
        let error_msg = Printf.sprintf "Type mismatch in binary operation: operator requires numeric types, found `%s` and `%s`"
          (string_of_simple_type left_type) (string_of_simple_type right_type) in
        type_error table error_msg pos
    | And | Or ->
      if left_type = TBool && right_type = TBool then TBool
      else
        let error_msg = Printf.sprintf "Logical operators require boolean operands: expected `bool`, found `%s` and `%s`"
          (string_of_simple_type left_type) (string_of_simple_type right_type) in
        type_error table error_msg pos
    | _ -> left_type  (* Default for other operations *)
    )
  | UnaryOp (op, expr, pos) ->
    let expr_type = check_expression table expr in
    (match op with
    | Not ->
      if expr_type = TBool then TBool
      else
        let error_msg = "Logical NOT requires boolean operand" in
        type_error table error_msg pos
    | Neg ->
      if is_numeric_type expr_type then expr_type
      else
        let error_msg = "Numeric negation requires numeric operand" in
        type_error table error_msg pos
    | Ref -> TPointer expr_type
    | Deref ->
      (match expr_type with
      | TPointer inner_type -> inner_type
      | _ ->
        let error_msg = "Cannot dereference non-pointer type" in
        type_error table error_msg pos)
    | _ -> expr_type)
  | FunctionCall (func_expr, args, pos) ->
    (match func_expr with
    | Identifier (name, _) -> 
      (match SymbolTable.lookup_function table name with
      | Some (param_types, return_type, _) ->
        let arg_types = List.map (check_expression table) args in
        if List.length param_types = List.length arg_types then (
          (* Check parameter type compatibility *)
          List.iter2 (fun expected actual ->
            if not (types_compatible expected actual) then
              let error_msg = Printf.sprintf "Function `%s` expects different argument types: expected `%s`, found `%s`"
                name (string_of_simple_type expected) (string_of_simple_type actual) in
              type_error table error_msg pos
          ) param_types arg_types;
          return_type
        ) else
          let error_msg = Printf.sprintf "Function '%s': wrong number of arguments (expected %d, got %d)"
            name (List.length param_types) (List.length arg_types) in
          type_error table error_msg pos
      | None ->
        let error_msg = Printf.sprintf "Undefined function '%s'" name in
        type_error table error_msg pos)
    | PathExpr (path, _) -> 
      let func_name = String.concat "::" path in
      (match SymbolTable.lookup_function table func_name with
      | Some (param_types, return_type, _) ->
        let arg_types = List.map (check_expression table) args in
        if List.length param_types = List.length arg_types then
          return_type
        else
          let error_msg = Printf.sprintf "Function '%s' expects %d arguments, got %d"
            func_name (List.length param_types) (List.length arg_types) in
          type_error table error_msg pos
      | None ->
        let error_msg = Printf.sprintf "Undefined function '%s'" func_name in
        type_error table error_msg pos)
    | FieldAccess (struct_expr, method_name, _) ->
      (* Method call: resolve struct type and look up method *)
      let struct_type = check_expression table struct_expr in
      (match struct_type with
      | TStruct (struct_name, _) ->
        let method_func_name = struct_name ^ "::" ^ method_name in
        (match SymbolTable.lookup_function table method_func_name with
        | Some (param_types, return_type, _) ->
          (* First parameter should be self *)
          let expected_args = List.tl param_types in
          let arg_types = List.map (check_expression table) args in
          if List.length expected_args = List.length arg_types then
            return_type
          else
            let error_msg = Printf.sprintf "Method '%s' expects %d arguments, got %d"
              method_name (List.length expected_args) (List.length arg_types) in
            type_error table error_msg pos
        | None ->
          let error_msg = Printf.sprintf "Undefined method '%s' for struct '%s'" method_name struct_name in
          type_error table error_msg pos)
      | _ ->
        let error_msg = "Method calls are only valid on struct types" in
        type_error table error_msg pos)
    | _ ->
      let error_msg = "Invalid function call expression" in
      type_error table error_msg pos)
  | FieldAccess (struct_expr, field_name, pos) ->
    let struct_type = check_expression table struct_expr in
    (match struct_type with
    | TStruct (struct_name, fields) ->
      (match List.assoc_opt field_name fields with
      | Some field_type -> field_type
      | None ->
        (* Try to look up struct definition *)
        (match SymbolTable.lookup_struct table struct_name with
        | Some (struct_fields, _) ->
          (match List.assoc_opt field_name struct_fields with
          | Some field_type -> field_type
          | None ->
            let error_msg = Printf.sprintf "Struct '%s' has no field '%s'" struct_name field_name in
            type_error table error_msg pos)
        | None ->
          let error_msg = Printf.sprintf "Unknown struct type '%s'" struct_name in
          type_error table error_msg pos))
    | _ ->
      let error_msg = Printf.sprintf "Field access on non-struct type: %s" 
        (string_of_simple_type struct_type) in
      type_error table error_msg pos)
  | Cast (expr, target_type, pos) ->
    let _ = check_expression table expr in
    ast_type_to_simple_type target_type
  | Index (array_expr, index_expr, pos) ->
    let array_type = check_expression table array_expr in
    let index_type = check_expression table index_expr in
    if not (is_integer_type index_type) then (
      let error_msg = "Array index must be an integer type" in
      type_error table error_msg pos
    );
    (match array_type with
    | TArray (element_type, _) -> element_type
    | TPointer element_type -> element_type
    | _ ->
      let error_msg = "Index operation requires array or pointer type" in
      type_error table error_msg pos)
  | StructExpr (path, field_exprs, pos) ->
    let struct_name = String.concat "::" path in
    (match SymbolTable.lookup_struct table struct_name with
    | Some (struct_fields, _) ->
      (* Check that all required fields are provided *)
      List.iter (fun (field_name, expected_type) ->
        match List.assoc_opt field_name field_exprs with
        | Some field_expr ->
          let actual_type = check_expression table field_expr in
          if not (types_compatible expected_type actual_type) then
            let error_msg = Printf.sprintf "Field '%s' type mismatch: expected %s, got %s"
              field_name (string_of_simple_type expected_type) (string_of_simple_type actual_type) in
            type_error table error_msg pos
        | None ->
          let error_msg = Printf.sprintf "Missing field '%s' in struct initialization" field_name in
          type_error table error_msg pos
      ) struct_fields;
      TStruct (struct_name, struct_fields)
    | None ->
      let error_msg = Printf.sprintf "Unknown struct type '%s'" struct_name in
      type_error table error_msg pos)
  | Return (expr_opt, pos) ->
    (match expr_opt with
    | Some expr -> check_expression table expr
    | None -> TUnit)
  | If (cond, then_block, else_block, pos) ->
    let cond_type = check_expression table cond in
    if cond_type <> TBool then (
      let error_msg = "If condition must be boolean" in
      type_error table error_msg pos
    );
    let (then_stmts, then_final) = then_block in
    let then_type = match then_final with
      | Some expr -> check_expression table expr
      | None -> TUnit
    in
    let else_type = match else_block with
      | Some (else_stmts, else_final) ->
        (match else_final with
        | Some expr -> check_expression table expr
        | None -> TUnit)
      | None -> TUnit
    in
    (* Both branches should have compatible types *)
    if types_compatible then_type else_type then then_type
    else if types_compatible else_type then_type then else_type
    else
      let error_msg = Printf.sprintf "If expression branches have incompatible types: `%s` and `%s`"
        (string_of_simple_type then_type) (string_of_simple_type else_type) in
      type_error table error_msg pos
  | Block (block, pos) ->
    let (stmts, final_expr) = block in
    (* Check all statements in the block *)
    SymbolTable.push_scope table BlockScope;
    (try
      List.iter (check_statement table) stmts;
      (* Return type of final expression *)
      let result = match final_expr with
        | Some expr -> check_expression table expr
        | None -> TUnit
      in
      SymbolTable.pop_scope table;
      result
    with
    | exn -> 
      SymbolTable.pop_scope table;
      raise exn)
  | _ ->
    (* Handle other expression types *)
    TUnit  (* Placeholder *)

(* Statement type checking with proper scoping *)
and check_statement table = function
  | LetStmt (is_mutable, name, type_opt, init_expr_opt) ->
    (* Extract position from initializer expression if available, otherwise use a default *)
    let pos = match init_expr_opt with
      | Some expr -> get_expression_position expr
      | None -> { Ast.line = 0; column = 0 }
    in
    let inferred_type = match init_expr_opt with
      | Some expr -> check_expression table expr
      | None -> 
        (match type_opt with
        | Some ast_type -> ast_type_to_simple_type ast_type
        | None ->
          let error_msg = "Variable declaration requires either type annotation or initializer" in
          semantic_error table error_msg pos)
    in
    let declared_type = match type_opt with
      | Some ast_type -> 
        let declared = ast_type_to_simple_type ast_type in
        if types_compatible declared inferred_type then declared
        else
          let error_msg = Printf.sprintf "Type mismatch in variable declaration: declared %s but expression has type %s"
            (string_of_simple_type declared) (string_of_simple_type inferred_type) in
          type_error table error_msg pos
      | None -> inferred_type
    in
    SymbolTable.declare_symbol table name declared_type is_mutable (ast_pos_to_error_pos pos) false

  | AssignStmt (lvalue, _, expr) ->
    (* Extract position from the expression being assigned *)
    let pos = get_expression_position expr in
    let expr_type = check_expression table expr in
    (match lvalue with
    | LvalueId name ->
      (match SymbolTable.lookup_symbol table name with
      | Some symbol ->
        if not symbol.is_mutable then (
          let error_msg = Printf.sprintf "Cannot assign to immutable variable '%s'" name in
          semantic_error table error_msg pos
        );
        if not (types_compatible symbol.symbol_type expr_type) then (
          let error_msg = Printf.sprintf "Type mismatch in assignment to '%s': expected `%s`, found `%s`"
            name (string_of_simple_type symbol.symbol_type) (string_of_simple_type expr_type) in
          type_error table error_msg pos
        )
      | None ->
        let error_msg = Printf.sprintf "Undefined variable '%s'" name in
        semantic_error table error_msg pos)
    | _ -> (* Handle other lvalue types *) ())

  | ExprStmt expr ->
    let _ = check_expression table expr in ()

  | ItemStmt item ->
    check_item table item

(* Item type checking *)
and check_item table = function
  | Function func_def ->
    (* Extract position from function body if possible *)
    let pos = (match func_def.func_body with
      | ([], Some expr) -> get_expression_position expr
      | (stmt :: _, _) -> (match stmt with
        | ExprStmt expr -> get_expression_position expr
        | LetStmt (_, _, _, Some expr) -> get_expression_position expr
        | _ -> { Ast.line = 0; column = 0 })
      | ([], None) -> { Ast.line = 0; column = 0 })
    in
    let param_types = List.map (fun param -> ast_type_to_simple_type param.param_type) func_def.func_params in
    let return_type = match func_def.func_return with
      | Some ret_type -> ast_type_to_simple_type ret_type
      | None -> TUnit
    in
    
    (* Register function in symbol table *)
    SymbolTable.register_function table func_def.func_name param_types return_type (ast_pos_to_error_pos pos);
    
    (* Enter function scope and check body *)
    SymbolTable.enter_function table func_def.func_name;
    
    (* Declare parameters *)
    List.iter (fun param ->
      let param_type = ast_type_to_simple_type param.param_type in
      let param_pos = { Ast.line = 0; column = 0 } in  (* TODO: Get actual position *)
      SymbolTable.declare_symbol table param.param_name param_type false (ast_pos_to_error_pos param_pos) true
    ) func_def.func_params;
    
    (* Check function body *)
    let (statements, final_expr) = func_def.func_body in
    List.iter (check_statement table) statements;
    
    (* Check final expression type matches return type *)
    let has_return_stmt = List.exists (function
      | ExprStmt (Return (_, _)) -> true
      | _ -> false
    ) statements in
    
    (match final_expr with
    | Some expr ->
      let actual_return_type = check_expression table expr in
      if not (types_compatible return_type actual_return_type) then
        let error_msg = Printf.sprintf "Function return type mismatch: expected %s, got %s"
          (string_of_simple_type return_type) (string_of_simple_type actual_return_type) in
        type_error table error_msg pos
    | None when has_return_stmt ->
      (* Function has return statements, so that's fine *)
      ()
    | None ->
      if return_type <> TUnit then
        let error_msg = Printf.sprintf "Function should return %s but has no return expression"
          (string_of_simple_type return_type) in
        semantic_error table error_msg pos);
    
    SymbolTable.exit_function table

  | Struct struct_def ->
    let pos = { Ast.line = 0; column = 0 } in  (* TODO: Get actual position *)
    let fields = List.map (fun field ->
      (field.field_name, ast_type_to_simple_type field.field_type)
    ) struct_def.struct_fields in
    SymbolTable.register_struct table struct_def.struct_name fields (ast_pos_to_error_pos pos)

  | Enum enum_def ->
    let pos = { Ast.line = 0; column = 0 } in  (* TODO: Get actual position *)
    let variants = List.map (fun variant -> variant.variant_name) enum_def.enum_variants in
    SymbolTable.register_enum table enum_def.enum_name variants (ast_pos_to_error_pos pos)

  | Impl impl_def ->
    (* Handle impl blocks - register methods *)
    let impl_type_name = match impl_def.impl_type with
      | PathType (path, _) -> String.concat "::" path
      | _ -> "Unknown"
    in
    
    List.iter (fun impl_item ->
      match impl_item with
      | ImplFunction func_def ->
        let pos = { Ast.line = 0; column = 0 } in  (* TODO: Get actual position *)
        let param_types = List.map (fun param -> 
          match param.param_type with
          | SelfType _ -> 
            (* Resolve self type to pointer to impl type *)
            TPointer (TStruct (impl_type_name, []))
          | other_type -> ast_type_to_simple_type other_type
        ) func_def.func_params in
        let return_type = match func_def.func_return with
          | Some ret_type -> ast_type_to_simple_type ret_type
          | None -> TUnit
        in
        let method_name = impl_type_name ^ "::" ^ func_def.func_name in
        SymbolTable.register_function table method_name param_types return_type (ast_pos_to_error_pos pos)
      | _ -> ()
    ) impl_def.impl_items

  | GlobalVar (_, is_static, is_mutable, name, type_opt, init_expr) ->
    let pos = get_expression_position init_expr in
    let inferred_type = check_expression table init_expr in
    let declared_type = match type_opt with
      | Some ast_type -> 
        let declared = ast_type_to_simple_type ast_type in
        if types_compatible declared inferred_type then declared
        else
          let error_msg = Printf.sprintf "Type mismatch in global variable declaration: declared %s but expression has type %s"
            (string_of_simple_type declared) (string_of_simple_type inferred_type) in
          type_error table error_msg pos
      | None -> inferred_type
    in
    (* Register the global variable in the global scope *)
    SymbolTable.declare_symbol table name declared_type is_mutable (ast_pos_to_error_pos pos) false

  | _ -> (* Handle other item types *) ()

(* Main type checking entry point *)
let type_check_program_with_symbols program filename =
  try
    let table = SymbolTable.create () in
    SymbolTable.set_filename table filename;
    
    (* Initialize built-in functions *)
    initialize_builtin_functions table;
    
    (* Type check all items *)
    List.iter (fun item ->
      check_item table item
    ) program;
    
    (* Check for unused variables and report warnings *)
    let warnings = SymbolTable.check_unused_variables table in
    List.iter (fun warning ->
      Printf.eprintf "Warning: %s\n" (Error.show_error_with_context warning)
    ) warnings;
    table
  with
  | Error.CompilerError error ->
    (* Re-raise the original error *)
    raise (Error.CompilerError error)
  | Failure msg ->
    let pos = { Error.line = 1; column = 1; offset = 0 } in
    let span = Error.make_span pos pos (Some filename) in
    let error = Error.make_error (Error.TypeError msg) span msg in
    raise (Error.CompilerError error)
  | Not_found ->
    let pos = { Error.line = 1; column = 1; offset = 0 } in
    let span = Error.make_span pos pos (Some filename) in
    let error = Error.make_error (Error.TypeError "Symbol not found during type checking") span "Symbol not found during type checking" in
    raise (Error.CompilerError error)
  | exn ->
    let pos = { Error.line = 1; column = 1; offset = 0 } in
    let span = Error.make_span pos pos (Some filename) in
    let error = Error.make_error (Error.UnknownError (Printexc.to_string exn)) span (Printexc.to_string exn) in
    raise (Error.CompilerError error)

(* Compatibility wrapper for existing code *)
let type_check_program program filename =
  let _ = type_check_program_with_symbols program filename in
  ()

(* Legacy compatibility - simplified interface *)
let set_filename filename = 
  ()
