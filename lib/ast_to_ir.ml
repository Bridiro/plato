open Ast
open Ir

(* Convert an AST program to IR module *)
let ast_program_to_ir_module (program : Ast.program) : ir_module =
  let ctx = create_ir_context () in
  let globals = ref [] in
  let functions = ref [] in
  let structs = ref [] in
  let enums = ref [] in
  
  (* Process each top-level item *)
  let rec process_item = function
    | Function func_def ->
      let ir_func = convert_function ctx func_def in
      functions := ir_func :: !functions
    | Struct struct_def ->
      let struct_name = struct_def.struct_name in
      let fields = List.map (fun field -> 
        (field.field_name, ast_type_to_ir_type field.field_type)) struct_def.struct_fields in
      structs := (struct_name, fields) :: !structs
    | Enum enum_def ->
      let enum_name = enum_def.enum_name in
      let variants = List.map (fun variant -> variant.variant_name) enum_def.enum_variants in
      enums := (enum_name, variants) :: !enums
    | GlobalVar (_, is_static, is_mutable, name, type_opt, init_expr) ->
      let ir_type = match type_opt with
        | Some t -> ast_type_to_ir_type t
        | None -> (* infer from initializer *) IntType 32 (* placeholder *)
      in
      let initial_value = Some (expression_to_ir_value ctx init_expr) in
      let global = {
        name = name;
        ir_type = ir_type;
        is_mutable = is_mutable;
        initial_value = initial_value;
      } in
      globals := global :: !globals
    | Impl impl_def -> 
      (* Process implementation items *)
      List.iter (fun impl_item ->
        match impl_item with
        | ImplFunction func_def ->
          (* Create a qualified name for impl functions *)
          let impl_type_name = match impl_def.impl_type with
            | PathType (path, _) -> String.concat "::" path
            | _ -> "Unknown"
          in
          let qualified_func_def = {
            func_def with 
            func_name = impl_type_name ^ "::" ^ func_def.func_name
          } in
          let ir_func = convert_function ctx qualified_func_def in
          functions := ir_func :: !functions
        | _ -> ()
      ) impl_def.impl_items
    | _ -> (* Other items not yet supported *)
      ()
  
  (* Convert a function definition to IR *)
  and convert_function ctx func_def =
    let params = List.map (fun param -> 
      (param.param_name, ast_type_to_ir_type param.param_type)) func_def.func_params in
    let return_type = match func_def.func_return with
      | Some rt -> ast_type_to_ir_type rt
      | None -> VoidType
    in
    
    (* Convert function body (block) to IR basic blocks *)
    let (statements, final_expr) = func_def.func_body in
    let entry_block = convert_statements_to_block ctx "entry" statements final_expr in
    
    {
      name = func_def.func_name;
      params = params;
      return_type = return_type;
      locals = []; (* Will be populated during conversion *)
      blocks = [entry_block];
    }
  
  (* Convert statements to a basic block *)
  and convert_statements_to_block ctx label statements final_expr =
    let instructions = ref [] in
    
    (* Convert each statement *)
    List.iter (fun stmt ->
      match stmt with
      | LetStmt (is_mutable, name, type_opt, init_expr_opt) ->
        (match init_expr_opt with
        | Some init_expr ->
          let ir_value = expression_to_ir_value ctx init_expr in
          instructions := Assign (name, ir_value) :: !instructions
        | None -> ())
      | AssignStmt (lvalue, assign_op, expr) ->
        let ir_value = expression_to_ir_value ctx expr in
        let var_name = match lvalue with
          | LvalueId name -> name
          | LvalueIndex (LvalueId array_name, index_expr) ->
            (* For array assignment like arr[1] = 4, we need special handling *)
            let index_ir = expression_to_ir_value ctx index_expr in
            let array_access = ArrayAccess (Variable array_name, index_ir) in
            let temp_name = generate_temp ctx in
            instructions := Store (array_access, ir_value) :: !instructions;
            temp_name (* Return temp name, though it won't be used *)
          | _ -> failwith "Complex lvalues not yet supported"
        in
        (match lvalue with
        | LvalueId _ -> instructions := Assign (var_name, ir_value) :: !instructions
        | LvalueIndex _ -> () (* Already handled above *)
        | _ -> ())
      | ExprStmt expr ->
        (* Handle different expression types appropriately *)
        (match expr with
        | If (cond, then_block, else_block, _) ->
          (* Convert if expression to proper control flow *)
          let cond_ir = expression_to_ir_value ctx cond in
          let then_label = generate_label ctx "then" in
          let else_label = generate_label ctx "else" in
          
          (* Branch instruction *)
          instructions := Branch (cond_ir, then_label, else_label) :: !instructions;
          
          (* For now, we'll handle simple if statements, but this needs improvement *)
          (* This is a placeholder - proper basic block handling needs to be implemented *)
          let temp_name = generate_temp ctx in
          instructions := Assign (temp_name, Constant 0) :: !instructions
        | _ ->
          let ir_value = expression_to_ir_value ctx expr in
          (* For other expression statements, we might need to generate a temporary *)
          let temp_name = generate_temp ctx in
          instructions := Assign (temp_name, ir_value) :: !instructions
        )
      | _ -> (* Other statements not yet supported *)
        ()
    ) statements;
    
    (* Handle final expression (if any) *)
    (match final_expr with
    | Some expr ->
      let ir_value = expression_to_ir_value ctx expr in
      instructions := Return (Some ir_value) :: !instructions
    | None ->
      instructions := Return None :: !instructions);
    
    {
      label = label;
      instructions = List.rev !instructions;
    }
  
  in
  
  (* Process all items in the program *)
  List.iter process_item program;
  
  {
    globals = List.rev !globals;
    functions = List.rev !functions;
    structs = List.rev !structs;
    enums = List.rev !enums;
  }

(* Main entry point for AST to IR conversion *)
let convert_ast_to_ir (program : Ast.program) : ir_module =
  ast_program_to_ir_module program
