open Ir

(* LLVM IR generation from our internal IR *)

(* Context for position tracking during LLVM generation *)
type llvm_gen_context = {
  mutable current_filename: string option;
  mutable current_position: Error.position option;
  (* Position mapping for IR constructs - maps unique IDs to positions *)
  mutable ir_positions: (string, Error.position) Hashtbl.t;
}

let create_llvm_context () = {
  current_filename = None;
  current_position = None;
  ir_positions = Hashtbl.create 100;
}

(* Global context for LLVM generation *)
let global_llvm_context = create_llvm_context ()

(* Global counter for unique IR construct IDs *)
let ir_id_counter = ref 0
let generate_ir_id () = 
  incr ir_id_counter;
  "ir_" ^ string_of_int !ir_id_counter

(* Position tracking functions *)
let track_ir_position ir_id pos =
  Hashtbl.replace global_llvm_context.ir_positions ir_id pos

let get_ir_position ir_id =
  Hashtbl.find_opt global_llvm_context.ir_positions ir_id

(* Error reporting functions using Error module *)
let llvm_gen_error_with_context message =
  let pos = match global_llvm_context.current_position with
    | Some p -> p
    | None -> Error.make_position 0 0 0  (* Default position if none available *)
  in
  let span = Error.make_span pos pos global_llvm_context.current_filename in
  let error = Error.make_error (Error.LlvmGenError message) span message in
  raise (Error.CompilerError error)

(* Enhanced error function that can use tracked positions *)
let llvm_gen_error_at_ir ir_construct message =
  let pos = match ir_construct with
    | Ir.FieldAccess _ -> 
      (* Try to find position in our tracking system or use current position *)
      (match global_llvm_context.current_position with
       | Some p -> p
       | None -> Error.make_position 0 0 0)
    | _ -> 
      (match global_llvm_context.current_position with
       | Some p -> p
       | None -> Error.make_position 0 0 0)
  in
  let span = Error.make_span pos pos global_llvm_context.current_filename in
  let error = Error.make_error (Error.LlvmGenError message) span message in
  raise (Error.CompilerError error)

let set_llvm_context_filename filename =
  global_llvm_context.current_filename <- Some filename

let set_llvm_context_position pos =
  global_llvm_context.current_position <- Some (Error.make_position pos.Ast.line pos.Ast.column 0)

(* Global context for struct information - temporary solution *)
let current_structs = ref []

(* Keep track of current function parameters for struct pointer lookup *)
let current_function_params = ref []

(* Type tracking system - maps temporary variable names to their IR types *)
let temp_types : (string, Ir.ir_type) Hashtbl.t = Hashtbl.create 100

(* Function signature registry - maps function names to their return types *)
let function_signatures : (string, Ir.ir_type) Hashtbl.t = Hashtbl.create 50

(* Helper function to find field index from global context *)
let find_field_index struct_name field_name structs =
  try
    let (_, fields) = List.find (fun (name, _) -> name = struct_name) structs in
    let rec find_index fields idx =
      match fields with
      | [] -> failwith (Printf.sprintf "Field '%s' not found in struct '%s'. Available fields: %s" 
                               field_name struct_name 
                               (String.concat ", " (List.map fst fields)))
      | (fname, _) :: rest -> 
        if fname = field_name then idx else find_index rest (idx + 1)
    in
    find_index fields 0
  with Not_found -> 
    let available_structs = List.map fst structs in
    failwith (Printf.sprintf "Struct '%s' not found. Available structs: %s" 
                     struct_name (String.concat ", " available_structs))

(* Helper function to determine struct type and field index *)
let get_struct_field_info field =
  (* Look through all structs to find one that has this field *)
  let rec find_struct_with_field structs =
    match structs with
    | [] -> 
      let all_fields = List.fold_left (fun acc (sname, fields) ->
        acc @ List.map (fun (fname, _) -> sname ^ "." ^ fname) fields
      ) [] !current_structs in
      failwith (Printf.sprintf "Field '%s' not found in any struct. Available fields: %s" 
                       field (String.concat ", " all_fields))
    | (sname, fields) :: rest ->
      try
        let idx = find_field_index sname field !current_structs in
        (sname, idx)
      with Failure _ -> find_struct_with_field rest
  in
  find_struct_with_field !current_structs

(* Helper function to get field type from struct definition *)
let get_field_type struct_name field_name =
  let fields = List.assoc struct_name !current_structs in
  let (_, field_type) = List.find (fun (name, _) -> name = field_name) fields in
  field_type

(* Function to infer the type of an IR value *)
let rec infer_ir_value_type = function
  | Constant i -> if i >= -2147483648 && i <= 2147483647 then IntType 32 else IntType 64
  | FloatConstant _ -> FloatType 64
  | BoolConstant _ -> BoolType
  | StringConstant _ -> StringType
  | CharConstant _ -> CharType
  | Variable name -> 
    (* Check if it's a temporary with known type *)
    (match Hashtbl.find_opt temp_types name with
    | Some ty -> ty
    | None -> 
      (* Check if it's a function parameter *)
      List.assoc name !current_function_params)
  | FieldAccess (struct_val, field) ->
    (* Get the struct type and look up field type *)
    let struct_type = infer_ir_value_type struct_val in
    (match struct_type with
    | StructType (struct_name, _) -> get_field_type struct_name field
    | _ -> 
      let actual_type = match struct_type with
        | IntType i -> Printf.sprintf "IntType %d" i
        | FloatType i -> Printf.sprintf "FloatType %d" i
        | BoolType -> "BoolType"
        | StringType -> "StringType"
        | _ -> "unknown type"
      in
      llvm_gen_error_at_ir (FieldAccess (struct_val, field)) (Printf.sprintf "Field access on non-struct type. Got %s, expected struct type" actual_type))
  | BinaryOp (left, op, _) ->
    let left_type = infer_ir_value_type left in
    (match op with
    | IEq | INe | ILt | IGt | ILe | IGe -> BoolType  (* Comparisons return bool *)
    | _ -> left_type)  (* Arithmetic operations preserve left operand type *)
  | UnaryOp (op, operand) ->
    let operand_type = infer_ir_value_type operand in
    (match op with
    | INot -> BoolType
    | INeg -> operand_type
    | IDeref -> (match operand_type with 
      | PointerType t -> t 
      | _ -> llvm_gen_error_with_context (Printf.sprintf "Cannot dereference non-pointer type: %s" 
               (match operand_type with 
                | IntType i -> Printf.sprintf "IntType %d" i
                | FloatType i -> Printf.sprintf "FloatType %d" i 
                | BoolType -> "BoolType"
                | _ -> "unknown type")))
    | IRef -> PointerType operand_type
    | ISizeof -> IntType 64)  (* sizeof typically returns size_t which is usually 64-bit *)
  | Cast (_, target_type) -> target_type
  | Call (func_name, _) -> 
    (* Look up function return type from function signatures registry *)
    (match Hashtbl.find_opt function_signatures func_name with
    | Some t -> t
    | None -> 
      let available_functions = Hashtbl.fold (fun k _ acc -> k :: acc) function_signatures [] in
      llvm_gen_error_with_context (Printf.sprintf "Function '%s' not found in signatures registry. Available functions: %s" 
                       func_name (String.concat ", " available_functions)))
  | _ -> llvm_gen_error_with_context "Cannot infer type for this IR value - unsupported IR construct"

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
  | _ -> failwith "LLVM IR Generation Error - Unsupported IR type conversion to LLVM type - this IR type is not yet implemented"

(* Generate LLVM value from IR value *)
let rec ir_value_to_llvm_value = function
  | Constant i -> string_of_int i
  | FloatConstant f -> string_of_float f
  | BoolConstant true -> "true"
  | BoolConstant false -> "false"
  | StringConstant s -> "\"" ^ String.escaped s ^ "\""
  | CharConstant c -> string_of_int (Char.code c)
  | Variable name -> 
    (* For struct parameters, use the _ptr version that was allocated and stored to *)
    if List.exists (fun (param_name, param_type) -> 
         param_name = name && 
         match param_type with StructType _ -> true | _ -> false
       ) !current_function_params then
      "%" ^ name ^ "_ptr"
    else
      "%" ^ name
  | Call (func_name, args) ->
    let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
    let args_str = String.concat ", " (List.map ir_value_to_llvm_value args) in
    "call @" ^ mangled_name ^ "(" ^ args_str ^ ")"
  | Load value -> 
    let val_type = infer_ir_value_type value in
    "load " ^ ir_type_to_llvm_type val_type ^ ", ptr " ^ ir_value_to_llvm_value value
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
    (match op, operand with
    | INeg, Constant n -> string_of_int (-n)  (* Constant folding for negation *)
    | INot, BoolConstant b -> if b then "false" else "true"  (* Constant folding for boolean not *)
    | _ ->
      (match op with
      | INot -> "xor i1 " ^ ir_value_to_llvm_value operand ^ ", true"
      | INeg -> 
        let operand_type = infer_ir_value_type operand in
        let type_str = ir_type_to_llvm_type operand_type in
        "sub " ^ type_str ^ " 0, " ^ ir_value_to_llvm_value operand
      | IDeref -> "load ptr, " ^ ir_value_to_llvm_value operand
      | IRef -> ir_value_to_llvm_value operand  (* Just return the address of the variable *)
      | ISizeof -> "8"))  (* sizeof typically returns size_t which is usually 64-bit, so 8 bytes *)
  | Cast (value, target_type) ->
    let target_llvm = ir_type_to_llvm_type target_type in
    (* Use appropriate cast instruction based on target type *)
    let cast_instr = match target_type with
      | FloatType _ -> "sitofp"  (* signed integer to floating point *)
      | IntType _ -> "trunc"     (* truncate or extend integer *)
      | _ -> "bitcast"           (* fallback for pointer casts *)
    in
    cast_instr ^ " " ^ ir_value_to_llvm_value value ^ " to " ^ target_llvm
  | FieldAccess (struct_val, field) ->
    let struct_ptr = ir_value_to_llvm_value struct_val in
    let (struct_name, field_index) = get_struct_field_info field in
    "getelementptr inbounds %struct." ^ struct_name ^ ", ptr " ^ struct_ptr ^ ", i32 0, i32 " ^ string_of_int field_index
  | ArrayAccess (array, index) ->
    "getelementptr inbounds " ^ ir_value_to_llvm_value array ^ ", " ^ ir_value_to_llvm_value index
  | StructInit (struct_name, fields) ->
    let field_values = List.map (fun (field_name, value) -> 
      match value with
      | StructInit (inner_struct_name, _) -> 
        "%struct." ^ inner_struct_name ^ " " ^ ir_value_to_llvm_value value
      | _ -> 
        let field_type = get_field_type struct_name field_name in
        ir_type_to_llvm_type field_type ^ " " ^ ir_value_to_llvm_value value
    ) fields in
    "{ " ^ String.concat ", " field_values ^ " }"
  | ArrayInit values ->
    let value_strs = List.map ir_value_to_llvm_value values in
    "[ " ^ String.concat ", " value_strs ^ " ]"
  | Select (cond, true_val, false_val) ->
    "select i1 " ^ ir_value_to_llvm_value cond ^ ", i32 " ^ ir_value_to_llvm_value true_val ^ ", i32 " ^ ir_value_to_llvm_value false_val
  | Store (addr, value) ->
    "store " ^ ir_value_to_llvm_value value ^ ", " ^ ir_value_to_llvm_value addr

(* Generate LLVM instruction from IR instruction *)
let ir_instruction_to_llvm temp_counter = function
  | Assign (var, value) ->
    (* Check if this is a phi node assignment from conditional evaluation *)
    if String.contains var ':' then
      (* Extract phi information: var format is "PHI:then_temp:then_label:else_temp:else_label" *)
      let parts = String.split_on_char ':' var in
      match parts with
      | ["PHI"; then_temp; then_label; else_temp; else_label] ->
        let actual_var = ir_value_to_llvm_value value in
        Printf.sprintf "%s = phi i32 [ %%%s, %%%s ], [ %%%s, %%%s ]"
          actual_var then_temp then_label else_temp else_label
      | _ ->
        (* Fallback for malformed phi *)
        let temp_name = "%" ^ var in
        temp_name ^ " = " ^ ir_value_to_llvm_value value
    else
      let temp_name = "%" ^ var in
      (match value with
      | Call (func_name, args) ->
        let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
        let call_return_type = match Hashtbl.find_opt function_signatures func_name with
          | Some t -> t
          | None -> 
            let available_functions = Hashtbl.fold (fun k _ acc -> k :: acc) function_signatures [] in
            failwith (Printf.sprintf "LLVM IR Generation Error - Function '%s' not found in signatures registry. Available functions: %s" 
                             func_name (String.concat ", " available_functions))
        in
        let call_return_type_str = ir_type_to_llvm_type call_return_type in
        let args_str = String.concat ", " (List.mapi (fun i arg -> 
          (* For method calls (containing ::), first argument is a pointer *)
          if String.contains func_name ':' && i = 0 then
            "ptr " ^ ir_value_to_llvm_value arg
          else
            let arg_type = infer_ir_value_type arg in
            ir_type_to_llvm_type arg_type ^ " " ^ ir_value_to_llvm_value arg) args) in
        temp_name ^ " = call " ^ call_return_type_str ^ " @" ^ mangled_name ^ "(" ^ args_str ^ ")"
    | BinaryOp (left, op, right) ->
      (* Helper function to load field access values recursively for assignments *)
      let rec get_loaded_value val_expr =
        match val_expr with
        | FieldAccess (struct_val, field) ->
          (* Handle nested field access by first resolving the struct_val *)
          let (base_instr, base_ptr) = match struct_val with
            | FieldAccess (nested_struct, nested_field) ->
              (* This is nested field access, resolve the base first *)
              let (nested_struct_name, nested_field_index) = get_struct_field_info nested_field in
              let nested_struct_ptr = ir_value_to_llvm_value nested_struct in
              let intermediate_temp = "%field_ptr_tmp" ^ string_of_int (!temp_counter) in
              incr temp_counter;
              let instr = intermediate_temp ^ " = getelementptr inbounds %struct." ^ nested_struct_name ^ ", ptr " ^ nested_struct_ptr ^ ", i32 0, i32 " ^ string_of_int nested_field_index in
              (instr, intermediate_temp)
            | _ ->
              (* Direct field access *)
              ("", ir_value_to_llvm_value struct_val)
          in
          let (struct_name, field_index) = get_struct_field_info field in
          let field_ptr_temp = "%field_ptr_tmp" ^ string_of_int (!temp_counter) in
          incr temp_counter;
          let load_temp = "%load_tmp" ^ string_of_int (!temp_counter) in
          incr temp_counter;
          let field_instr = field_ptr_temp ^ " = getelementptr inbounds %struct." ^ struct_name ^ ", ptr " ^ base_ptr ^ ", i32 0, i32 " ^ string_of_int field_index in
          let field_type = get_field_type struct_name field in
          let load_instr = load_temp ^ " = load " ^ ir_type_to_llvm_type field_type ^ ", ptr " ^ field_ptr_temp in
          (* Track the type of this loaded value *)
          Hashtbl.replace temp_types (String.sub load_temp 1 (String.length load_temp - 1)) field_type;
          let all_instr = String.concat "\n  " (List.filter (fun s -> s <> "") [base_instr; field_instr; load_instr]) in
          (all_instr, load_temp, field_type)
        | Call (func_name, args) ->
          (* Generate separate call instruction for function calls *)
          let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
          let call_temp = "%call_tmp" ^ string_of_int (!temp_counter) in
          incr temp_counter;
          let args_instrs = List.map (fun arg -> get_loaded_value arg) args in
          let all_arg_instrs = String.concat "\n  " (List.filter (fun s -> s <> "") (List.map (fun (i, _, _) -> i) args_instrs)) in
          let arg_values = List.map (fun (_, v, _) -> v) args_instrs in
          let call_return_type = match Hashtbl.find_opt function_signatures func_name with
            | Some t -> t
            | None -> failwith ("Function " ^ func_name ^ " not found in signatures registry")
          in
          let call_return_type_str = ir_type_to_llvm_type call_return_type in
          let args_str = String.concat ", " (List.map (fun v -> 
            (* Get type from temporary types registry or fail if unknown *)
            let val_type = try
              Hashtbl.find temp_types (String.sub v 1 (String.length v - 1))
            with Not_found -> failwith ("Unknown temporary variable type: " ^ v)
            in
            ir_type_to_llvm_type val_type ^ " " ^ v
          ) arg_values) in
          let call_instr = call_temp ^ " = call " ^ call_return_type_str ^ " @" ^ mangled_name ^ "(" ^ args_str ^ ")" in
          let final_instr = if all_arg_instrs = "" then call_instr else all_arg_instrs ^ "\n  " ^ call_instr in
          let call_type = match Hashtbl.find_opt function_signatures func_name with
            | Some t -> t
            | None -> failwith ("Function " ^ func_name ^ " not found in signatures registry")
          in
          Hashtbl.replace temp_types (String.sub call_temp 1 (String.length call_temp - 1)) call_type;
          (final_instr, call_temp, call_type)
        | BinaryOp (left, nested_op, right) ->
          (* Handle nested binary operations *)
          let (left_instr, left_val, left_type) = get_loaded_value left in
          let (right_instr, right_val, right_type) = get_loaded_value right in
          let result_type = left_type in  (* Use left operand type for result *)
          let nested_temp = "%nested_tmp" ^ string_of_int (!temp_counter) in
          incr temp_counter;
          (* Generate appropriate operation based on operand types *)
          let (nested_op_str, type_str) = match result_type with
            | FloatType _ -> 
              let op_str = match nested_op with
              | IAdd -> "fadd"
              | ISub -> "fsub" 
              | IMul -> "fmul"
              | IDiv -> "fdiv"
              | _ -> "fadd"
              in (op_str, "double")
            | _ -> 
              let int_op = match nested_op with
              | IAdd -> "add" | ISub -> "sub" | IMul -> "mul" | IDiv -> "sdiv" | IMod -> "srem"
              | _ -> "add"
              in 
              let type_str = ir_type_to_llvm_type result_type in
              (int_op, type_str)
          in
          let all_nested_instr = String.concat "\n  " (List.filter (fun s -> s <> "") [left_instr; right_instr]) in
          let nested_binary_instr = nested_temp ^ " = " ^ nested_op_str ^ " " ^ type_str ^ " " ^ left_val ^ ", " ^ right_val in
          let final_nested_instr = if all_nested_instr = "" then nested_binary_instr else all_nested_instr ^ "\n  " ^ nested_binary_instr in
          Hashtbl.replace temp_types (String.sub nested_temp 1 (String.length nested_temp - 1)) result_type;
          (final_nested_instr, nested_temp, result_type)
        | _ -> 
          let val_type = infer_ir_value_type val_expr in
          ("", ir_value_to_llvm_value val_expr, val_type)
      in
      let (left_instr, left_val, left_type) = get_loaded_value left in
      let (right_instr, right_val, right_type) = get_loaded_value right in
      (* Use the left operand type to determine the result type and operation *)
      let result_type = left_type in
      let (op_str, type_str) = match result_type with
        | FloatType _ -> 
          let float_op = match op with
          | IAdd -> "fadd"
          | ISub -> "fsub"  
          | IMul -> "fmul"
          | IDiv -> "fdiv"
          | _ -> "fadd"
          in (float_op, "double")
        | _ -> 
          let int_op = match op with
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
          let type_str = ir_type_to_llvm_type result_type in
          (int_op, type_str)
      in
      let all_instr = String.concat "\n  " (List.filter (fun s -> s <> "") [left_instr; right_instr]) in
      let final_instr = if all_instr = "" then "" else all_instr ^ "\n  " in
      (* Track the type of the result *)
      Hashtbl.replace temp_types (String.sub temp_name 1 (String.length temp_name - 1)) result_type;
      final_instr ^ temp_name ^ " = " ^ op_str ^ " " ^ type_str ^ " " ^ left_val ^ ", " ^ right_val
    | UnaryOp (op, operand) ->
      (match op with
      | INot -> temp_name ^ " = xor i1 " ^ ir_value_to_llvm_value operand ^ ", true"
      | INeg -> 
        let operand_type = infer_ir_value_type operand in
        let type_str = ir_type_to_llvm_type operand_type in
        temp_name ^ " = sub " ^ type_str ^ " 0, " ^ ir_value_to_llvm_value operand
      | IDeref -> 
        let operand_type = infer_ir_value_type operand in
        let deref_type = match operand_type with
          | PointerType t -> t
          | _ -> failwith "Dereferencing non-pointer type"
        in
        let type_str = ir_type_to_llvm_type deref_type in
        temp_name ^ " = load " ^ type_str ^ ", ptr " ^ ir_value_to_llvm_value operand
      | IRef -> 
        let operand_type = infer_ir_value_type operand in
        let type_str = ir_type_to_llvm_type operand_type in
        temp_name ^ " = alloca " ^ type_str
      | ISizeof -> 
        temp_name ^ " = add i64 0, 8")  (* sizeof returns size_t which is typically 64-bit *)
    | FieldAccess (struct_val, field) ->
      let (struct_name, field_index) = get_struct_field_info field in
      temp_name ^ " = getelementptr inbounds %struct." ^ struct_name ^ ", ptr " ^ ir_value_to_llvm_value struct_val ^ ", i32 0, i32 " ^ string_of_int field_index
    | ArrayAccess (array, index) ->
      temp_name ^ " = getelementptr inbounds [10 x i32], ptr " ^ ir_value_to_llvm_value array ^ ", i32 0, i32 " ^ ir_value_to_llvm_value index
    | StructInit (struct_name, fields) ->
      (* For struct initialization, create and store each field separately *)
      let struct_alloc = temp_name ^ " = alloca %struct." ^ struct_name in
      let field_stores = List.mapi (fun i (field_name, fval) ->
        let field_ptr_temp = "%field_ptr" ^ string_of_int (!temp_counter) in
        incr temp_counter;
        let gep_instr = field_ptr_temp ^ " = getelementptr inbounds %struct." ^ struct_name ^ ", ptr " ^ temp_name ^ ", i32 0, i32 " ^ string_of_int i in
        match fval with
        | StructInit (inner_struct_name, _) ->
          (* For nested structs, store the entire struct value *)
          let store_instr = "store %struct." ^ inner_struct_name ^ " " ^ ir_value_to_llvm_value fval ^ ", ptr " ^ field_ptr_temp in
          gep_instr ^ "\n  " ^ store_instr
        | _ ->
          (* Get the proper type for this field *)
          let field_type = get_field_type struct_name field_name in
          let type_str = ir_type_to_llvm_type field_type in
          (* Check if the field value is a struct that needs to be loaded *)
          let value_str = match field_type with
            | StructType (struct_type_name, _) ->
              (* This field expects a struct value, so we need to load it if fval is a variable *)
              (match fval with
              | Variable var_name ->
                (* Load the struct from the variable pointer *)
                let load_temp = "%load_tmp" ^ string_of_int (!temp_counter) in
                incr temp_counter;
                let load_instr = load_temp ^ " = load %struct." ^ struct_type_name ^ ", ptr " ^ ir_value_to_llvm_value fval in
                let store_instr = "store " ^ type_str ^ " " ^ load_temp ^ ", ptr " ^ field_ptr_temp in
                load_instr ^ "\n  " ^ store_instr
              | _ ->
                "store " ^ type_str ^ " " ^ ir_value_to_llvm_value fval ^ ", ptr " ^ field_ptr_temp)
            | _ ->
              "store " ^ type_str ^ " " ^ ir_value_to_llvm_value fval ^ ", ptr " ^ field_ptr_temp
          in
          gep_instr ^ "\n  " ^ value_str
      ) fields in
      struct_alloc ^ "\n  " ^ String.concat "\n  " field_stores
    | Cast (value, target_type) ->
      let target_llvm = ir_type_to_llvm_type target_type in
      let cast_instr = match target_type with
        | FloatType _ -> "sitofp"  (* signed integer to floating point *)
        | IntType _ -> "trunc"     (* truncate or extend integer *)
        | _ -> "bitcast"           (* fallback for pointer casts *)
      in
      (* Handle field access in cast source *)
      (match value with
      | BinaryOp (left, op, right) ->
        (* For cast of binary operations, first compute the binary op, then cast *)
        let binary_temp = "%binary_tmp" ^ string_of_int (!temp_counter) in
        incr temp_counter;
        let op_str = match op with
          | IMul -> "mul"
          | IAdd -> "add"
          | ISub -> "sub"
          | _ -> "add"  (* fallback *)
        in
        (* Get loaded values for binary operation *)
        let get_loaded_value val_expr =
          match val_expr with
          | FieldAccess (struct_val, field) ->
            let struct_ptr = ir_value_to_llvm_value struct_val in
            let (struct_name, field_index) = get_struct_field_info field in
            let field_ptr_temp = "%field_ptr_tmp" ^ string_of_int (!temp_counter) in
            incr temp_counter;
            let load_temp = "%load_tmp" ^ string_of_int (!temp_counter) in
            incr temp_counter;
            let field_type = get_field_type struct_name field in
            let load_type_str = ir_type_to_llvm_type field_type in
            let instr = field_ptr_temp ^ " = getelementptr inbounds %struct." ^ struct_name ^ ", ptr " ^ struct_ptr ^ ", i32 0, i32 " ^ string_of_int field_index ^ "\n  " ^
                       load_temp ^ " = load " ^ load_type_str ^ ", ptr " ^ field_ptr_temp in
            (instr, load_temp)
          | _ -> ("", ir_value_to_llvm_value val_expr)
        in
        let (left_instr, left_val) = get_loaded_value left in
        let (right_instr, right_val) = get_loaded_value right in
        let all_instr = String.concat "\n  " (List.filter (fun s -> s <> "") [left_instr; right_instr]) in
        (* Use proper type inference for binary operations in casts *)
        let left_type = infer_ir_value_type left in
        let type_str = ir_type_to_llvm_type left_type in
        let binary_instr = binary_temp ^ " = " ^ op_str ^ " " ^ type_str ^ " " ^ left_val ^ ", " ^ right_val in
        let cast_instr_final = temp_name ^ " = " ^ cast_instr ^ " " ^ type_str ^ " " ^ binary_temp ^ " to " ^ target_llvm in
        (if all_instr = "" then "" else all_instr ^ "\n  ") ^ binary_instr ^ "\n  " ^ cast_instr_final
      | _ ->
        temp_name ^ " = " ^ cast_instr ^ " " ^ ir_value_to_llvm_value value ^ " to " ^ target_llvm)
    | _ ->
      (* Use proper type inference instead of hardcoded i32 *)
      let val_type = infer_ir_value_type value in
      let type_str = ir_type_to_llvm_type val_type in
      temp_name ^ " = add " ^ type_str ^ " 0, " ^ ir_value_to_llvm_value value)
  
  | Branch (cond, then_label, else_label) ->
    (* Handle comparison conditions properly *)
    (match cond with
    | BinaryOp (left, op, right) when op = ILe || op = ILt || op = IGe || op = IGt || op = IEq || op = INe ->
      (* Generate comparison instruction first *)
      let op_str = match op with
        | IEq -> "icmp eq"
        | INe -> "icmp ne"
        | ILt -> "icmp slt"
        | IGt -> "icmp sgt"
        | ILe -> "icmp sle"
        | IGe -> "icmp sge"
        | _ -> "icmp eq"  (* fallback *)
      in
      let cmp_temp = "%cmp_tmp" ^ string_of_int (!temp_counter) in
      incr temp_counter;
      cmp_temp ^ " = " ^ op_str ^ " i32 " ^ ir_value_to_llvm_value left ^ ", " ^ ir_value_to_llvm_value right ^ "\n  " ^
      "br i1 " ^ cmp_temp ^ ", label %" ^ then_label ^ ", label %" ^ else_label
    | _ ->
      "br i1 " ^ ir_value_to_llvm_value cond ^ ", label %" ^ then_label ^ ", label %" ^ else_label)
  
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
      (* Look up function return type from function signatures registry *)
      let return_type = match Hashtbl.find_opt function_signatures func_name with
        | Some t -> t
        | None -> failwith ("Function " ^ func_name ^ " not found in signatures registry")
      in
      let return_type_str = ir_type_to_llvm_type return_type in
      temp_name ^ " = call " ^ return_type_str ^ " @" ^ mangled_name ^ "(" ^ args_str ^ ")\n  ret " ^ return_type_str ^ " " ^ temp_name
    | FieldAccess (struct_val, field) ->
      let struct_ptr = ir_value_to_llvm_value struct_val in
      let (struct_name, field_index) = get_struct_field_info field in
      let field_ptr_temp = "%field_ptr_tmp" ^ string_of_int (!temp_counter) in
      incr temp_counter;
      let load_temp = "%ret_tmp" ^ string_of_int (!temp_counter) in
      incr temp_counter;
      let field_type = get_field_type struct_name field in
      let load_type_str = ir_type_to_llvm_type field_type in
      let return_type_str = load_type_str in
      field_ptr_temp ^ " = getelementptr inbounds %struct." ^ struct_name ^ ", ptr " ^ struct_ptr ^ ", i32 0, i32 " ^ string_of_int field_index ^ "\n  " ^
      load_temp ^ " = load " ^ load_type_str ^ ", ptr " ^ field_ptr_temp ^ "\n  ret " ^ return_type_str ^ " " ^ load_temp
    | StructInit (struct_name, fields) ->
      (* For struct returns, we need to build the struct properly *)
      let struct_temp = "%struct_temp" ^ string_of_int (!temp_counter) in
      incr temp_counter;
      let alloc_instr = struct_temp ^ " = alloca %struct." ^ struct_name in
      
      (* Generate store instructions for each field *)
      let store_instrs = List.mapi (fun i (field_name, fval) ->
        let field_ptr_temp = "%field_ptr" ^ string_of_int (!temp_counter) in
        incr temp_counter;
        let gep_instr = field_ptr_temp ^ " = getelementptr inbounds %struct." ^ struct_name ^ ", ptr " ^ struct_temp ^ ", i32 0, i32 " ^ string_of_int i in
        (* Get the proper type for this field *)
        let field_type = get_field_type struct_name field_name in
        let type_str = ir_type_to_llvm_type field_type in
        let store_instr = "store " ^ type_str ^ " " ^ ir_value_to_llvm_value fval ^ ", ptr " ^ field_ptr_temp in
        gep_instr ^ "\n  " ^ store_instr
      ) fields in
      
      (* Load the complete struct and return it *)
      let load_temp = "%load_temp" ^ string_of_int (!temp_counter) in
      incr temp_counter;
      let load_instr = load_temp ^ " = load %struct." ^ struct_name ^ ", ptr " ^ struct_temp in
      
      alloc_instr ^ "\n  " ^ String.concat "\n  " store_instrs ^ "\n  " ^ load_instr ^ "\n  ret %struct." ^ struct_name ^ " " ^ load_temp
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
      (* Helper function to load field access values recursively for returns *)
      let rec get_loaded_value val_expr =
        match val_expr with
        | FieldAccess (struct_val, field) ->
          let struct_ptr = ir_value_to_llvm_value struct_val in
          let (struct_name, field_index) = get_struct_field_info field in
          let field_ptr_temp = "%field_ptr_tmp" ^ string_of_int (!temp_counter) in
          incr temp_counter;
          let load_temp = "%load_tmp" ^ string_of_int (!temp_counter) in
          incr temp_counter;
          let field_type = get_field_type struct_name field in
          let load_type_str = ir_type_to_llvm_type field_type in
          let instr = field_ptr_temp ^ " = getelementptr inbounds %struct." ^ struct_name ^ ", ptr " ^ struct_ptr ^ ", i32 0, i32 " ^ string_of_int field_index ^ "\n  " ^
                     load_temp ^ " = load " ^ load_type_str ^ ", ptr " ^ field_ptr_temp in
          (instr, load_temp)
        | Call (func_name, args) ->
          (* Generate separate call instruction for function calls *)
          let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
          let call_temp = "%call_tmp" ^ string_of_int (!temp_counter) in
          incr temp_counter;
          let args_instrs = List.map (fun arg -> get_loaded_value arg) args in
          let all_arg_instrs = String.concat "\n  " (List.filter (fun s -> s <> "") (List.map fst args_instrs)) in
          let arg_values = List.map snd args_instrs in
          let call_return_type = match Hashtbl.find_opt function_signatures func_name with
            | Some t -> t
            | None -> failwith ("Function " ^ func_name ^ " not found in signatures registry")
          in
          let call_return_type_str = ir_type_to_llvm_type call_return_type in
          let args_str = String.concat ", " (List.map (fun v -> 
            (* Get type from temporary types registry or fail if unknown *)
            let val_type = try
              Hashtbl.find temp_types (String.sub v 1 (String.length v - 1))
            with Not_found -> failwith ("Unknown temporary variable type: " ^ v)
            in
            ir_type_to_llvm_type val_type ^ " " ^ v
          ) arg_values) in
          let call_instr = call_temp ^ " = call " ^ call_return_type_str ^ " @" ^ mangled_name ^ "(" ^ args_str ^ ")" in
          let final_instr = if all_arg_instrs = "" then call_instr else all_arg_instrs ^ "\n  " ^ call_instr in
          (final_instr, call_temp)
        | BinaryOp (left, nested_op, right) ->
          (* Handle nested binary operations *)
          let nested_op_str = match nested_op with
            | IAdd -> "add" | ISub -> "sub" | IMul -> "mul" | IDiv -> "sdiv" | IMod -> "srem"
            | _ -> "add"  (* fallback *)
          in
          let (left_instr, left_val) = get_loaded_value left in
          let (right_instr, right_val) = get_loaded_value right in
          let nested_temp = "%nested_tmp" ^ string_of_int (!temp_counter) in
          incr temp_counter;
          let all_nested_instr = String.concat "\n  " (List.filter (fun s -> s <> "") [left_instr; right_instr]) in
          (* Use proper type inference for nested binary operations *)
          let left_type = infer_ir_value_type left in
          let type_str = ir_type_to_llvm_type left_type in
          let nested_binary_instr = nested_temp ^ " = " ^ nested_op_str ^ " " ^ type_str ^ " " ^ left_val ^ ", " ^ right_val in
          let final_nested_instr = if all_nested_instr = "" then nested_binary_instr else all_nested_instr ^ "\n  " ^ nested_binary_instr in
          (final_nested_instr, nested_temp)
        | _ -> ("", ir_value_to_llvm_value val_expr)
      in
      let (left_instr, left_val) = get_loaded_value left in
      let (right_instr, right_val) = get_loaded_value right in
      (* Infer types from the operands *)
      let left_type = infer_ir_value_type left in
      let result_type = left_type in  (* Use left operand type *)
      (* Generate appropriate operation based on operand types *)
      let (final_op_str, type_str) = match result_type with
        | FloatType _ -> 
          let float_op = match op with
          | IAdd -> "fadd"
          | ISub -> "fsub"  
          | IMul -> "fmul"
          | IDiv -> "fdiv"
          | _ -> "fadd"
          in (float_op, "double")
        | _ -> 
          let type_str = ir_type_to_llvm_type result_type in
          (op_str, type_str)
      in
      let temp_name = "%ret_tmp" ^ string_of_int (!temp_counter) in
      incr temp_counter;
      let all_instr = String.concat "\n  " (List.filter (fun s -> s <> "") [left_instr; right_instr]) in
      let final_instr = if all_instr = "" then "" else all_instr ^ "\n  " in
      let return_type_str = ir_type_to_llvm_type result_type in
      final_instr ^ temp_name ^ " = " ^ final_op_str ^ " " ^ type_str ^ " " ^ left_val ^ ", " ^ right_val ^ "\n  ret " ^ return_type_str ^ " " ^ temp_name
    | _ ->
      let val_type = infer_ir_value_type value in
      "ret " ^ ir_type_to_llvm_type val_type ^ " " ^ ir_value_to_llvm_value value)
  
  | Return None ->
    "ret void"
  
  | Label label ->
    label ^ ":"
  
  | Call (func_name, args) ->
    let mangled_name = String.map (function ':' -> '_' | c -> c) func_name in
    let call_return_type = match Hashtbl.find_opt function_signatures func_name with
      | Some t -> t
      | None -> failwith ("Function " ^ func_name ^ " not found in signatures registry")
    in
    let call_return_type_str = ir_type_to_llvm_type call_return_type in
    let args_str = String.concat ", " (List.mapi (fun i arg -> 
      (* For method calls (containing ::), first argument is a pointer *)
      if String.contains func_name ':' && i = 0 then
        "ptr " ^ ir_value_to_llvm_value arg
      else
        let arg_type = infer_ir_value_type arg in
        ir_type_to_llvm_type arg_type ^ " " ^ ir_value_to_llvm_value arg) args) in
    "call " ^ call_return_type_str ^ " @" ^ mangled_name ^ "(" ^ args_str ^ ")"
  
  | Store (addr, value) ->
    let val_type = infer_ir_value_type value in
    "store " ^ ir_type_to_llvm_type val_type ^ " " ^ ir_value_to_llvm_value value ^ ", ptr " ^ ir_value_to_llvm_value addr

(* Generate LLVM function from IR function *)
let ir_function_to_llvm ir_func =
  (* Set current function parameters for struct pointer lookup *)
  current_function_params := ir_func.params;
  
  (* Register local variables in temp_types for type lookup *)
  List.iter (fun (name, ir_type) ->
    Hashtbl.replace temp_types name ir_type
  ) ir_func.locals;
  
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
    
    (* Add parameter stores for struct types only to the entry block *)
    let param_stores =
      if block.label = "entry" then
        List.filter_map (fun (name, ty) ->
          match ty with
          | StructType _ ->
            (* Store struct parameter to its allocated pointer *)
            Some ("  store " ^ ir_type_to_llvm_type ty ^ " %" ^ name ^ ", ptr %" ^ name ^ "_ptr")
          | _ -> None
        ) ir_func.params
      else []
    in
    
    let instructions_str = String.concat "\n  " (List.map (ir_instruction_to_llvm temp_counter) block.instructions) in
    let all_instructions = locals_instructions @ param_stores @ ["  " ^ instructions_str] in
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
  let mutability = if global.is_mutable then "global" else "constant" in
  let initial_val = match global.initial_value with
    | Some value -> ir_value_to_llvm_value value
    | None -> "0"  (* Default initialization *)
  in
  "@" ^ global.name ^ " = " ^ mutability ^ " " ^ global_type ^ " " ^ initial_val

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
  (* Set global struct context for field access *)
  current_structs := ir_module.structs;
  (* Register function signatures for proper return type lookup *)
  List.iter (fun (ir_func : Ir.ir_function) ->
    Hashtbl.replace function_signatures ir_func.name ir_func.return_type
  ) ir_module.functions;
  ir_module_to_llvm ir_module
