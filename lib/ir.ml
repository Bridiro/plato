(* Re-define operations to avoid dependency issues *)
type ir_binary_op =
  | IAdd | ISub | IMul | IDiv | IMod
  | IEq | INe | ILt | IGt | ILe | IGe
  | IAnd | IOr
  | IBitAnd | IBitOr | IBitXor | IShl | IShr

type ir_unary_op =
  | INot | INeg | IDeref | IRef | ISizeof

(* IR type system - simplified from AST types *)
type ir_type = 
  | IntType of int        (* bit width: 8, 16, 32, 64 *)
  | UIntType of int       (* unsigned bit width: 8, 16, 32, 64 *)
  | FloatType of int      (* bit width: 32, 64 *)
  | BoolType
  | CharType
  | StringType
  | VoidType
  | UnitType
  | PointerType of ir_type
  | ArrayType of ir_type * int
  | FunctionType of ir_type list * ir_type
  | StructType of string * (string * ir_type) list
  | EnumType of string * string list

(* IR values represent operands in instructions *)
type ir_value =
  | Constant of int
  | FloatConstant of float
  | BoolConstant of bool
  | StringConstant of string
  | CharConstant of char
  | Variable of string
  | Call of string * ir_value list
  | Load of ir_value
  | Store of ir_value * ir_value
  | BinaryOp of ir_value * ir_binary_op * ir_value
  | UnaryOp of ir_unary_op * ir_value
  | Cast of ir_value * ir_type
  | FieldAccess of ir_value * string
  | ArrayAccess of ir_value * ir_value
  | StructInit of string * (string * ir_value) list
  | ArrayInit of ir_value list
  | Select of ir_value * ir_value * ir_value  (* condition, true_value, false_value *)

(* IR instructions represent operations *)
type ir_instruction =
  | Assign of string * ir_value
  | Branch of ir_value * string * string
  | Jump of string
  | Return of ir_value option
  | Label of string
  | Call of string * ir_value list
  | Store of ir_value * ir_value  (* Store value to address *)

(* Basic blocks for control flow *)
type ir_basic_block = {
  label: string;
  instructions: ir_instruction list;
}

(* Function representation in IR *)
type ir_function = {
  name: string;
  params: (string * ir_type) list;
  return_type: ir_type;
  locals: (string * ir_type) list;
  blocks: ir_basic_block list;
}

(* Global variable representation *)
type ir_global = {
  name: string;
  ir_type: ir_type;
  is_mutable: bool;
  initial_value: ir_value option;
}

(* Complete IR module representation *)
type ir_module = {
  globals: ir_global list;
  functions: ir_function list;
  structs: (string * (string * ir_type) list) list;
  enums: (string * string list) list;
}

(* Convert AST binary operations to IR binary operations *)
let ast_binary_op_to_ir = function
  | Ast.Add -> IAdd
  | Ast.Sub -> ISub
  | Ast.Mul -> IMul
  | Ast.Div -> IDiv
  | Ast.Mod -> IMod
  | Ast.Eq -> IEq
  | Ast.Ne -> INe
  | Ast.Lt -> ILt
  | Ast.Gt -> IGt
  | Ast.Le -> ILe
  | Ast.Ge -> IGe
  | Ast.And -> IAnd
  | Ast.Or -> IOr
  | Ast.BitAnd -> IBitAnd
  | Ast.BitOr -> IBitOr
  | Ast.BitXor -> IBitXor
  | Ast.Shl -> IShl
  | Ast.Shr -> IShr

(* Convert AST unary operations to IR unary operations *)
let ast_unary_op_to_ir = function
  | Ast.Not -> INot
  | Ast.Neg -> INeg
  | Ast.Deref -> IDeref
  | Ast.Ref -> IRef
  | Ast.Sizeof -> ISizeof

(* Convert AST primitive types to IR types *)
let primitive_to_ir_type = function
  | Ast.I8 -> IntType 8
  | Ast.I16 -> IntType 16
  | Ast.I32 -> IntType 32
  | Ast.I64 -> IntType 64
  | Ast.U8 -> UIntType 8
  | Ast.U16 -> UIntType 16
  | Ast.U32 -> UIntType 32
  | Ast.U64 -> UIntType 64
  | Ast.Usize -> UIntType 64  (* Assume 64-bit target *)
  | Ast.F32 -> FloatType 32
  | Ast.F64 -> FloatType 64
  | Ast.Bool -> BoolType
  | Ast.Char -> CharType
  | Ast.Str -> StringType
  | Ast.Void -> VoidType

(* Convert AST types to IR types *)
(* Context-aware AST type to IR type conversion *)
let rec ast_type_to_ir_type_with_context impl_type_opt = function
  | Ast.PrimType prim -> primitive_to_ir_type prim
  | Ast.ArrayType (element_type, _size_expr) ->
    (* For now, assume constant size - will need evaluation later *)
    let ir_element_type = ast_type_to_ir_type_with_context impl_type_opt element_type in
    ArrayType (ir_element_type, 1)  (* Placeholder size *)
  | Ast.PointerType inner_type ->
    PointerType (ast_type_to_ir_type_with_context impl_type_opt inner_type)
  | Ast.FunctionType (param_types, return_type) ->
    let ir_param_types = List.map (ast_type_to_ir_type_with_context impl_type_opt) param_types in
    let ir_return_type = match return_type with
      | Some rt -> ast_type_to_ir_type_with_context impl_type_opt rt
      | None -> VoidType
    in
    FunctionType (ir_param_types, ir_return_type)
  | Ast.PathType (path, _) ->
    (* For now, assume it's a struct type *)
    StructType (String.concat "::" path, [])
  | Ast.GenericType name ->
    (* Generics should be monomorphized before IR generation *)
    failwith ("Generic type not monomorphized: " ^ name)
  | Ast.SelfType _ ->
    (* Resolve SelfType based on impl context *)
    (match impl_type_opt with
    | Some impl_type ->
      (match impl_type with
      | Ast.PathType (path, _) ->
        (* self is a pointer to the struct type *)
        let struct_name = String.concat "::" path in
        PointerType (StructType (struct_name, []))
      | _ -> failwith "SelfType in non-struct impl not supported")
    | None -> failwith "SelfType without impl context")

let rec ast_type_to_ir_type = function
  | Ast.PrimType prim -> primitive_to_ir_type prim
  | Ast.ArrayType (element_type, _size_expr) ->
    (* For now, assume constant size - will need evaluation later *)
    let ir_element_type = ast_type_to_ir_type element_type in
    ArrayType (ir_element_type, 1)  (* Placeholder size *)
  | Ast.PointerType inner_type ->
    PointerType (ast_type_to_ir_type inner_type)
  | Ast.FunctionType (param_types, return_type) ->
    let ir_param_types = List.map ast_type_to_ir_type param_types in
    let ir_return_type = match return_type with
      | Some rt -> ast_type_to_ir_type rt
      | None -> VoidType
    in
    FunctionType (ir_param_types, ir_return_type)
  | Ast.PathType (path, _) ->
    (* For now, assume it's a struct type *)
    StructType (String.concat "::" path, [])
  | Ast.GenericType name ->
    (* Generics should be monomorphized before IR generation *)
    failwith ("Generic type not monomorphized: " ^ name)
  | Ast.SelfType _ ->
    (* SelfType should be resolved during type checking phase *)
    failwith "SelfType should be resolved before IR generation"

(* Convert AST literals to IR values *)
let literal_to_ir_value = function
  | Ast.IntLit (value, _) -> Constant value
  | Ast.FloatLit (value, _) -> FloatConstant value
  | Ast.StringLit value -> StringConstant value
  | Ast.CharLit value -> CharConstant value
  | Ast.BoolLit value -> BoolConstant value
  | Ast.UnitLit -> Constant 0  (* Unit as null/void *)
  | Ast.NullLit -> Constant 0  (* Null pointer as 0 *)

(* Context for IR generation *)
type ir_context = {
  current_function: string option;
  locals: (string * ir_type) list;
  label_counter: int ref;
  temp_counter: int ref;
}

let create_ir_context () = {
  current_function = None;
  locals = [];
  label_counter = ref 0;
  temp_counter = ref 0;
}

(* Generate unique labels *)
let generate_label ctx prefix =
  let counter = !(ctx.label_counter) in
  ctx.label_counter := counter + 1;
  prefix ^ "_" ^ string_of_int counter

(* Generate unique temporary variable names *)
let generate_temp ctx =
  let counter = !(ctx.temp_counter) in
  ctx.temp_counter := counter + 1;
  "tmp_" ^ string_of_int counter

(* Convert AST expressions to IR values *)
let rec expression_to_ir_value ctx = function
  | Ast.Literal (lit, _) -> literal_to_ir_value lit
  | Ast.Identifier (name, _) -> Variable name
  | Ast.PathExpr (path, _) -> Variable (String.concat "::" path)
  | Ast.BinaryOp (left, op, right, _) ->
    let left_ir = expression_to_ir_value ctx left in
    let right_ir = expression_to_ir_value ctx right in
    let ir_op = ast_binary_op_to_ir op in
    BinaryOp (left_ir, ir_op, right_ir)
  | Ast.UnaryOp (op, expr, _) ->
    let expr_ir = expression_to_ir_value ctx expr in
    let ir_op = ast_unary_op_to_ir op in
    UnaryOp (ir_op, expr_ir)
  | Ast.Cast (expr, target_type, _) ->
    let expr_ir = expression_to_ir_value ctx expr in
    let target_ir_type = ast_type_to_ir_type target_type in
    Cast (expr_ir, target_ir_type)
  | Ast.Index (array, index, _) ->
    let array_ir = expression_to_ir_value ctx array in
    let index_ir = expression_to_ir_value ctx index in
    ArrayAccess (array_ir, index_ir)
  | Ast.FieldAccess (expr, field, _) ->
    let expr_ir = expression_to_ir_value ctx expr in
    FieldAccess (expr_ir, field)
  | Ast.FunctionCall (func_expr, args, _) ->
    let func_name = match func_expr with
      | Ast.Identifier (name, _) -> name
      | Ast.PathExpr (path, _) -> String.concat "::" path
      | _ -> failwith "Complex function expressions not yet supported"
    in
    let args_ir = List.map (expression_to_ir_value ctx) args in
    Call (func_name, args_ir)
  | Ast.ArrayExpr (elements, _) ->
    let elements_ir = List.map (expression_to_ir_value ctx) elements in
    ArrayInit elements_ir
  | Ast.StructExpr (path, fields, _) ->
    let struct_name = String.concat "::" path in
    let fields_ir = List.map (fun (name, expr) -> 
      (name, expression_to_ir_value ctx expr)) fields in
    StructInit (struct_name, fields_ir)
  | Ast.Block (block, _) ->
    (* For now, convert block to a placeholder - this needs proper handling *)
    Constant 0
  | Ast.If (cond, then_block, else_block, _) ->
    (* If expressions should be handled at the statement level, not as expressions *)
    failwith "If expressions should be handled as statements, not expressions"
  | Ast.While (cond, body, _) ->
    (* While loops should be handled at the statement level, not expressions *)
    failwith "While loops should be handled as statements, not expressions"
  | Ast.For (var, iter, body, _) ->
    (* For loops should be handled at the statement level, not expressions *)
    failwith "For loops should be handled as statements, not expressions"
  | Ast.Loop (body, _) ->
    (* Loop constructs should be handled at the statement level, not expressions *)
    failwith "Loop constructs should be handled as statements, not expressions"
  | Ast.Match (expr, arms, _) ->
    (* Match expressions should be handled at the statement level, not expressions *)
    failwith "Match expressions should be handled as statements, not expressions"
  | Ast.Range (start, end_expr, _) ->
    (* Range expressions are used in for loops - convert to a simple structure *)
    let start_ir = expression_to_ir_value ctx start in
    let end_ir = expression_to_ir_value ctx end_expr in
    (* For now, create a simple representation - this needs proper range handling *)
    ArrayInit [start_ir; end_ir]
  | Ast.Break (expr_opt, _) ->
    failwith "Break statements should be handled at the statement level, not expressions"
  | Ast.Continue _ ->
    failwith "Continue statements should be handled at the statement level, not expressions"
  | Ast.Return (expr_opt, _) ->
    (match expr_opt with
    | Some expr -> expression_to_ir_value ctx expr
    | None -> Constant 0)
  | _ -> failwith "Expression type not yet supported in IR conversion"

(* Pretty printing for IR *)
let rec string_of_ir_type = function
  | IntType bits -> "i" ^ string_of_int bits
  | UIntType bits -> "u" ^ string_of_int bits
  | FloatType bits -> "f" ^ string_of_int bits
  | BoolType -> "bool"
  | CharType -> "char"
  | StringType -> "string"
  | VoidType -> "void"
  | UnitType -> "unit"
  | PointerType inner -> "*" ^ string_of_ir_type inner
  | ArrayType (inner, size) -> "[" ^ string_of_ir_type inner ^ "; " ^ string_of_int size ^ "]"
  | FunctionType (params, ret) ->
    "fn(" ^ String.concat ", " (List.map string_of_ir_type params) ^ ") -> " ^ string_of_ir_type ret
  | StructType (name, _) -> "struct " ^ name
  | EnumType (name, _) -> "enum " ^ name

let rec string_of_ir_value = function
  | Constant i -> string_of_int i
  | FloatConstant f -> string_of_float f
  | BoolConstant b -> string_of_bool b
  | StringConstant s -> "\"" ^ s ^ "\""
  | CharConstant c -> "'" ^ String.make 1 c ^ "'"
  | Variable name -> name
  | Call (func, args) ->
    func ^ "(" ^ String.concat ", " (List.map string_of_ir_value args) ^ ")"
  | Load addr -> "load " ^ string_of_ir_value addr
  | Store (addr, value) ->
    "store " ^ string_of_ir_value value ^ " to " ^ string_of_ir_value addr
  | BinaryOp (left, op, right) ->
    string_of_ir_value left ^ " " ^ (match op with
      | IAdd -> "+"
      | ISub -> "-"
      | IMul -> "*"
      | IDiv -> "/"
      | IMod -> "%"
      | IEq -> "=="
      | INe -> "!="
      | ILt -> "<"
      | IGt -> ">"
      | ILe -> "<="
      | IGe -> ">="
      | IAnd -> "&&"
      | IOr -> "||"
      | IBitAnd -> "&"
      | IBitOr -> "|"
      | IBitXor -> "^"
      | IShl -> "<<"
      | IShr -> ">>") ^ " " ^ string_of_ir_value right
  | UnaryOp (op, expr) ->
    (match op with
      | INot -> "!" ^ string_of_ir_value expr
      | INeg -> "-" ^ string_of_ir_value expr
      | IDeref -> "*" ^ string_of_ir_value expr
      | IRef -> "&" ^ string_of_ir_value expr
      | ISizeof -> "sizeof(" ^ string_of_ir_value expr ^ ")")
  | Cast (expr, target_type) ->
    string_of_ir_value expr ^ " as " ^ string_of_ir_type target_type
  | FieldAccess (expr, field) ->
    string_of_ir_value expr ^ "." ^ field
  | ArrayAccess (array, index) ->
    string_of_ir_value array ^ "[" ^ string_of_ir_value index ^ "]"
  | StructInit (name, fields) ->
    name ^ " { " ^ String.concat ", " (List.map (fun (n, v) -> n ^ ": " ^ string_of_ir_value v) fields) ^ " }"
  | ArrayInit elements ->
    "[" ^ String.concat ", " (List.map string_of_ir_value elements) ^ "]"
  | Select (cond, true_val, false_val) ->
    "select " ^ string_of_ir_value cond ^ " ? " ^ string_of_ir_value true_val ^ " : " ^ string_of_ir_value false_val

let string_of_ir_instruction = function
  | Assign (var, value) -> var ^ " = " ^ string_of_ir_value value
  | Branch (cond, then_label, else_label) ->
    "br " ^ string_of_ir_value cond ^ " ? " ^ then_label ^ " : " ^ else_label
  | Jump label -> "jmp " ^ label
  | Return (Some value) -> "ret " ^ string_of_ir_value value
  | Return None -> "ret void"
  | Label name -> name ^ ":"
  | Call (func, args) ->
    func ^ "(" ^ String.concat ", " (List.map string_of_ir_value args) ^ ")"
  | Store (addr, value) ->
    "store " ^ string_of_ir_value value ^ " to " ^ string_of_ir_value addr

let string_of_ir_basic_block block =
  block.label ^ ":\n" ^
  String.concat "\n" (List.map (fun instr -> "  " ^ string_of_ir_instruction instr) block.instructions)

let string_of_ir_function func =
  let params_str = String.concat ", " (List.map (fun (name, ty) -> name ^ ": " ^ string_of_ir_type ty) func.params) in
  let header = "fn " ^ func.name ^ "(" ^ params_str ^ ") -> " ^ string_of_ir_type func.return_type ^ " {\n" in
  let blocks_str = String.concat "\n\n" (List.map string_of_ir_basic_block func.blocks) in
  header ^ blocks_str ^ "\n}\n"

let string_of_ir_module module_ir =
  let globals_str = String.concat "\n" (List.map (fun global ->
    let mutability = if global.is_mutable then "mut " else "" in
    "global " ^ mutability ^ global.name ^ ": " ^ string_of_ir_type global.ir_type) module_ir.globals) in
  
  let structs_str = String.concat "\n" (List.map (fun (name, fields) ->
    let fields_str = String.concat ", " (List.map (fun (field_name, field_type) ->
      field_name ^ ": " ^ string_of_ir_type field_type) fields) in
    "struct " ^ name ^ " { " ^ fields_str ^ " }") module_ir.structs) in
  
  let enums_str = String.concat "\n" (List.map (fun (name, variants) ->
    let variants_str = String.concat ", " variants in
    "enum " ^ name ^ " { " ^ variants_str ^ " }") module_ir.enums) in
  
  let functions_str = String.concat "\n\n" (List.map string_of_ir_function module_ir.functions) in
  
  let all_parts = List.filter (fun s -> s <> "") [globals_str; structs_str; enums_str; functions_str] in
  String.concat "\n\n" all_parts

(* IR validation *)
let validate_ir_module module_ir =
  (* TODO: Implement validation checks *)
  (* - Check that all referenced variables are defined *)
  (* - Check that all labels are defined *)
  (* - Check type consistency *)
  (* - Check that functions have proper entry/exit *)
  true
