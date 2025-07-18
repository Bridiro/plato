open Ir

(* LLVM IR generation from our internal IR *)

(* LLVM type mappings *)
let rec ir_type_to_llvm_type = function
  | IntType 8 -> "i8"
  | IntType 16 -> "i16"
  | IntType 32 -> "i32"
  | IntType 64 -> "i64"
  | UIntType 8 -> "i8"
  | UIntType 16 -> "i16"
  | UIntType 32 -> "i32"
  | UIntType 64 -> "i64"
  | FloatType 32 -> "float"
  | FloatType 64 -> "double"
  | BoolType -> "i1"
  | CharType -> "i8"
  | StringType -> "ptr"  (* LLVM 15+ opaque pointer *)
  | VoidType -> "void"
  | UnitType -> "void"
  | PointerType _ -> "ptr"  (* LLVM 15+ opaque pointers *)
  | ArrayType (elem_type, size) -> 
    "[" ^ string_of_int size ^ " x " ^ ir_type_to_llvm_type elem_type ^ "]"
  | FunctionType (param_types, return_type) ->
    let param_str = String.concat ", " (List.map ir_type_to_llvm_type param_types) in
    ir_type_to_llvm_type return_type ^ " (" ^ param_str ^ ")"
  | StructType (name, _) -> "%struct." ^ name
  | EnumType (name, _) -> "i32"  (* Enums as i32 for now *)
  | _ -> "i32"  (* Default fallback *)

(* Generate LLVM value from IR value *)
let rec ir_value_to_llvm_value = function
  | Constant i -> string_of_int i
  | FloatConstant f -> string_of_float f
  | BoolConstant true -> "true"
  | BoolConstant false -> "false"
  | StringConstant s -> "\"" ^ String.escaped s ^ "\""
  | CharConstant c -> string_of_int (Char.code c)
  | Variable name -> "%" ^ name
  | Call (func_name, args) ->
    let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
    let args_str = String.concat ", " (List.map ir_value_to_llvm_value args) in
    "call @" ^ mangled_name ^ "(" ^ args_str ^ ")"
  | Load value -> "load ptr, " ^ ir_value_to_llvm_value value
  | BinaryOp (left, op, right) ->
    let op_str = match op with
      | IAdd -> "add"
      | ISub -> "sub"
      | IMul -> "mul"
      | IDiv -> "sdiv"
      | IMod -> "srem"
      | IEq -> "icmp eq"
      | INe -> "icmp ne"
      | ILt -> "icmp slt"
      | IGt -> "icmp sgt"
      | ILe -> "icmp sle"
      | IGe -> "icmp sge"
      | IAnd -> "and"
      | IOr -> "or"
      | IBitAnd -> "and"
      | IBitOr -> "or"
      | IBitXor -> "xor"
      | IShl -> "shl"
      | IShr -> "ashr"
    in
    op_str ^ " " ^ ir_value_to_llvm_value left ^ ", " ^ ir_value_to_llvm_value right
  | UnaryOp (op, operand) ->
    (match op with
    | INot -> "xor i1 " ^ ir_value_to_llvm_value operand ^ ", true"
    | INeg -> "sub i32 0, " ^ ir_value_to_llvm_value operand
    | IDeref -> "load ptr, " ^ ir_value_to_llvm_value operand
    | IRef -> "alloca i32 ; address of " ^ ir_value_to_llvm_value operand
    | ISizeof -> "4")  (* Simplified sizeof *)
  | Cast (value, target_type) ->
    let target_llvm = ir_type_to_llvm_type target_type in
    "bitcast " ^ ir_value_to_llvm_value value ^ " to " ^ target_llvm
  | FieldAccess (struct_val, field) ->
    "getelementptr inbounds " ^ ir_value_to_llvm_value struct_val ^ ", " ^ field
  | ArrayAccess (array, index) ->
    "getelementptr inbounds " ^ ir_value_to_llvm_value array ^ ", " ^ ir_value_to_llvm_value index
  | StructInit (struct_name, fields) ->
    let field_values = List.map (fun (_, value) -> ir_value_to_llvm_value value) fields in
    "{ " ^ String.concat ", " field_values ^ " }"
  | ArrayInit values ->
    let value_strs = List.map ir_value_to_llvm_value values in
    "[ " ^ String.concat ", " value_strs ^ " ]"
  | Store (addr, value) ->
    "store " ^ ir_value_to_llvm_value value ^ ", " ^ ir_value_to_llvm_value addr

(* Generate LLVM instruction from IR instruction *)
let ir_instruction_to_llvm temp_counter = function
  | Assign (var, value) ->
    let temp_name = "%" ^ var in
    (match value with
    | Call (func_name, args) ->
      let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
      let args_str = String.concat ", " (List.map (fun arg -> 
        "i32 " ^ ir_value_to_llvm_value arg) args) in
      temp_name ^ " = call i32 @" ^ mangled_name ^ "(" ^ args_str ^ ")"
    | BinaryOp (left, op, right) ->
      let op_str = match op with
        | IAdd -> "add"
        | ISub -> "sub"
        | IMul -> "mul"
        | IDiv -> "sdiv"
        | IMod -> "srem"
        | IEq -> "icmp eq"
        | INe -> "icmp ne"
        | ILt -> "icmp slt"
        | IGt -> "icmp sgt"
        | ILe -> "icmp sle"
        | IGe -> "icmp sge"
        | IAnd -> "and"
        | IOr -> "or"
        | IBitAnd -> "and"
        | IBitOr -> "or"
        | IBitXor -> "xor"
        | IShl -> "shl"
        | IShr -> "ashr"
      in
      temp_name ^ " = " ^ op_str ^ " i32 " ^ ir_value_to_llvm_value left ^ ", " ^ ir_value_to_llvm_value right
    | UnaryOp (op, operand) ->
      (match op with
      | INot -> temp_name ^ " = xor i1 " ^ ir_value_to_llvm_value operand ^ ", true"
      | INeg -> temp_name ^ " = sub i32 0, " ^ ir_value_to_llvm_value operand
      | IDeref -> temp_name ^ " = load i32, ptr " ^ ir_value_to_llvm_value operand
      | IRef -> temp_name ^ " = alloca i32"
      | ISizeof -> temp_name ^ " = add i32 0, 4")
    | FieldAccess (struct_val, field) ->
      temp_name ^ " = getelementptr inbounds %struct, ptr " ^ ir_value_to_llvm_value struct_val ^ ", i32 0, i32 0"
    | ArrayAccess (array, index) ->
      temp_name ^ " = getelementptr inbounds [10 x i32], ptr " ^ ir_value_to_llvm_value array ^ ", i32 0, i32 " ^ ir_value_to_llvm_value index
    | StructInit (struct_name, fields) ->
      let field_values = List.map (fun (_, value) -> ir_value_to_llvm_value value) fields in
      temp_name ^ " = alloca %struct." ^ struct_name ^ "\n  store %struct." ^ struct_name ^ " { " ^ 
      String.concat ", " (List.map (fun fval -> "i32 " ^ fval) field_values) ^ " }, ptr " ^ temp_name
    | _ ->
      temp_name ^ " = add i32 0, " ^ ir_value_to_llvm_value value)
  
  | Branch (cond, then_label, else_label) ->
    "br i1 " ^ ir_value_to_llvm_value cond ^ ", label %" ^ then_label ^ ", label %" ^ else_label
  
  | Jump label ->
    "br label %" ^ label
  
  | Return (Some value) ->
    (match value with
    | Call (func_name, args) ->
      let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
      let args_str = String.concat ", " (List.map (fun arg -> 
        "ptr " ^ ir_value_to_llvm_value arg) args) in
      let temp_name = "%ret_tmp" ^ string_of_int (!temp_counter) in
      incr temp_counter;
      temp_name ^ " = call i32 @" ^ mangled_name ^ "(" ^ args_str ^ ")\n  ret i32 " ^ temp_name
    | StructInit (struct_name, fields) ->
      let field_values = List.map (fun (_, fval) -> ir_value_to_llvm_value fval) fields in
      "ret %struct." ^ struct_name ^ " { " ^ String.concat ", " (List.map2 (fun (_, ftype) fval -> 
        ir_type_to_llvm_type ftype ^ " " ^ fval) [("",(IntType 32)); ("",(IntType 32))] field_values) ^ " }"
    | BinaryOp (left, op, right) ->
      (* For binary operations in return, we need to compute first then return *)
      let op_str = match op with
        | IAdd -> "add"
        | ISub -> "sub"
        | IMul -> "mul"
        | IDiv -> "sdiv"
        | IMod -> "srem"
        | IEq -> "icmp eq"
        | INe -> "icmp ne"
        | ILt -> "icmp slt"
        | IGt -> "icmp sgt"
        | ILe -> "icmp sle"
        | IGe -> "icmp sge"
        | IAnd -> "and"
        | IOr -> "or"
        | IBitAnd -> "and"
        | IBitOr -> "or"
        | IBitXor -> "xor"
        | IShl -> "shl"
        | IShr -> "ashr"
      in
      let temp_name = "%ret_tmp" ^ string_of_int (!temp_counter) in
      incr temp_counter;
      temp_name ^ " = " ^ op_str ^ " i32 " ^ ir_value_to_llvm_value left ^ ", " ^ ir_value_to_llvm_value right ^ "\n  ret i32 " ^ temp_name
    | _ ->
      "ret i32 " ^ ir_value_to_llvm_value value)
  
  | Return None ->
    "ret void"
  
  | Label label ->
    label ^ ":"
  
  | Call (func_name, args) ->
    let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
    let args_str = String.concat ", " (List.map (fun arg -> 
      "i32 " ^ ir_value_to_llvm_value arg) args) in
    "call i32 @" ^ mangled_name ^ "(" ^ args_str ^ ")"
  
  | Store (addr, value) ->
    "store i32 " ^ ir_value_to_llvm_value value ^ ", ptr " ^ ir_value_to_llvm_value addr

(* Generate LLVM function from IR function *)
let ir_function_to_llvm ir_func =
  let return_type = ir_type_to_llvm_type ir_func.return_type in
  let params_str = String.concat ", " (List.map (fun (name, ty) ->
    ir_type_to_llvm_type ty ^ " %" ^ name) ir_func.params) in
  
  (* Mangle function names with :: to use _ instead *)
  let mangled_name = String.map (function ':' -> '_' | c -> c) ir_func.name in
  
  let header = "define " ^ return_type ^ " @" ^ mangled_name ^ "(" ^ params_str ^ ") {" in
  
  (* Generate basic blocks *)
  let temp_counter = ref 0 in
  let blocks_str = String.concat "\n" (List.map (fun block ->
    let block_header = block.label ^ ":" in
    
    (* Add local allocations only to the first block (entry) *)
    let locals_instructions = 
      if block.label = "entry" then
        List.map (fun (name, ty) ->
          "  %" ^ name ^ "_ptr = alloca " ^ ir_type_to_llvm_type ty) ir_func.locals
      else []
    in
    
    let instructions_str = String.concat "\n  " (List.map (ir_instruction_to_llvm temp_counter) block.instructions) in
    let all_instructions = locals_instructions @ ["  " ^ instructions_str] in
    block_header ^ "\n" ^ String.concat "\n" all_instructions
  ) ir_func.blocks) in
  
  header ^ "\n" ^ blocks_str ^ "\n}\n"

(* Generate LLVM struct type declaration *)
let ir_struct_to_llvm (struct_name, fields) =
  let field_types = List.map (fun (_, ty) -> ir_type_to_llvm_type ty) fields in
  let fields_str = String.concat ", " field_types in
  "%struct." ^ struct_name ^ " = type { " ^ fields_str ^ " }"

(* Generate LLVM global variable *)
let ir_global_to_llvm global =
  let global_type = ir_type_to_llvm_type global.ir_type in
  let mutability = if global.is_mutable then "" else "constant" in
  let initial_val = match global.initial_value with
    | Some value -> ir_value_to_llvm_value value
    | None -> "0"  (* Default initialization *)
  in
  "@" ^ global.name ^ " = global " ^ mutability ^ " " ^ global_type ^ " " ^ initial_val

(* Generate complete LLVM module from IR module *)
let ir_module_to_llvm ir_module =
  let target_triple = "target triple = \"arm64-apple-macosx11.0\"" in
  let data_layout = "target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128\"" in
  
  (* Generate struct declarations *)
  let structs_str = String.concat "\n" (List.map ir_struct_to_llvm ir_module.structs) in
  
  (* Generate global variables *)
  let globals_str = String.concat "\n" (List.map ir_global_to_llvm ir_module.globals) in
  
  (* Generate function declarations for external functions *)
  let external_decls = [
    "declare i32 @printf(ptr, ...)";
    "declare ptr @malloc(i64)";
    "declare void @free(ptr)";
    "declare double @sqrt(double)";
  ] in
  let external_str = String.concat "\n" external_decls in
  
  (* Generate functions *)
  let functions_str = String.concat "\n" (List.map ir_function_to_llvm ir_module.functions) in
  
  (* Combine all sections *)
  String.concat "\n\n" [
    data_layout;
    target_triple;
    structs_str;
    globals_str;
    external_str;
    functions_str;
  ]

(* Main entry point for LLVM generation *)
let generate_llvm ir_module =
  ir_module_to_llvm ir_module
