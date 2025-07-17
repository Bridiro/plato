open Ast
open Ir

(* Enhanced context for proper IR generation with real basic blocks *)
type conversion_context = {
  mutable label_counter: int;
  mutable temp_counter: int;
  mutable current_blocks: ir_basic_block list;
  mutable current_function: string option;
  (* Symbol table for proper scoping *)
  mutable scopes: (string, ir_type) Hashtbl.t list;
  (* Control flow context *)
  mutable break_label: string option;
  mutable continue_label: string option;
}

let create_conversion_context () = {
  label_counter = 0;
  temp_counter = 0;
  current_blocks = [];
  current_function = None;
  scopes = [Hashtbl.create 32]; (* Start with global scope *)
  break_label = None;
  continue_label = None;
}

(* Generate unique labels and temps *)
let generate_label ctx prefix =
  let counter = ctx.label_counter in
  ctx.label_counter <- counter + 1;
  prefix ^ "_" ^ string_of_int counter

let generate_temp ctx =
  let counter = ctx.temp_counter in
  ctx.temp_counter <- counter + 1;
  "tmp_" ^ string_of_int counter

(* Scope management *)
let push_scope ctx =
  let new_scope = Hashtbl.create 32 in
  ctx.scopes <- new_scope :: ctx.scopes

let pop_scope ctx =
  match ctx.scopes with
  | [] -> failwith "Cannot pop global scope"
  | _ :: rest -> ctx.scopes <- rest

let lookup_symbol ctx name =
  let rec search = function
    | [] -> None
    | scope :: rest ->
      (match Hashtbl.find_opt scope name with
      | Some ty -> Some ty
      | None -> search rest)
  in
  search ctx.scopes

let declare_symbol ctx name ir_type =
  match ctx.scopes with
  | [] -> failwith "No scope available"
  | current_scope :: _ -> Hashtbl.replace current_scope name ir_type

(* Block management *)
let add_block ctx block =
  ctx.current_blocks <- block :: ctx.current_blocks

let get_blocks_in_order ctx =
  List.rev ctx.current_blocks

let create_block_with_terminator label instructions terminator =
  { label = label; instructions = instructions @ [terminator] }

let create_empty_block label =
  { label = label; instructions = [] }

(* Block management *)
let add_block ctx block =
  ctx.current_blocks <- block :: ctx.current_blocks

let get_blocks_in_order ctx =
  List.rev ctx.current_blocks

(* Convert AST expressions to IR values with proper context *)
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
  | Ast.Range (start, end_expr, _) ->
    let start_ir = expression_to_ir_value ctx start in
    let end_ir = expression_to_ir_value ctx end_expr in
    ArrayInit [start_ir; end_ir]
  | Ast.PointerAccess (expr, field, _) ->
    let expr_ir = expression_to_ir_value ctx expr in
    FieldAccess (expr_ir, field)  (* Similar to field access for now *)
  | Ast.Block (block, _) ->
    (* For block expressions, we need to handle them specially *)
    Constant 0  (* Placeholder - blocks should be handled as statements *)
  | Ast.Return (expr_opt, _) ->
    (match expr_opt with
    | Some expr -> expression_to_ir_value ctx expr
    | None -> Constant 0)
  | Ast.Break (expr_opt, _) ->
    (match expr_opt with
    | Some expr -> expression_to_ir_value ctx expr
    | None -> Constant 0)
  | Ast.Continue _ ->
    Constant 0  (* Continue doesn't produce a value *)
  | _ -> failwith "Expression type not yet supported in IR conversion"

(* Convert statements to instructions, generating proper basic blocks *)
let rec convert_statement_to_blocks ctx stmt =
  match stmt with
  | LetStmt (is_mutable, name, type_opt, init_expr_opt) ->
    (match init_expr_opt with
    | Some init_expr ->
      let ir_value = expression_to_ir_value ctx init_expr in
      let ir_type = match type_opt with
        | Some t -> ast_type_to_ir_type t
        | None -> IntType 32 (* Infer type - placeholder *)
      in
      declare_symbol ctx name ir_type;
      [Assign (name, ir_value)]
    | None -> [])
  
  | AssignStmt (lvalue, assign_op, expr) ->
    let ir_value = expression_to_ir_value ctx expr in
    (match lvalue with
    | LvalueId name -> [Assign (name, ir_value)]
    | LvalueIndex (LvalueId array_name, index_expr) ->
      let index_ir = expression_to_ir_value ctx index_expr in
      let array_access = ArrayAccess (Variable array_name, index_ir) in
      [Store (array_access, ir_value)]
    | _ -> failwith "Complex lvalues not yet supported")
  
  | ExprStmt expr ->
    convert_expression_statement_to_blocks ctx expr

  | ItemStmt _ -> []

(* Convert expression statements with proper control flow *)
and convert_expression_statement_to_blocks ctx = function
  | If (cond, then_block, else_block, _) ->
    convert_if_to_blocks ctx cond then_block else_block
  
  | For (var, iter_expr, body, _) ->
    convert_for_to_blocks ctx var iter_expr body
  
  | While (cond, body, _) ->
    convert_while_to_blocks ctx cond body
  
  | Loop (body, _) ->
    convert_loop_to_blocks ctx body
  
  | expr ->
    (* For other expressions, just evaluate and assign to temp *)
    let ir_value = expression_to_ir_value ctx expr in
    let temp_name = generate_temp ctx in
    [Assign (temp_name, ir_value)]

(* Convert if statement to proper basic blocks *)
and convert_if_to_blocks ctx cond then_block else_block =
  let cond_ir = expression_to_ir_value ctx cond in
  let then_label = generate_label ctx "then" in
  let else_label = generate_label ctx "else" in
  let merge_label = generate_label ctx "merge" in
  
  (* Create then block *)
  let (then_stmts, then_final) = then_block in
  push_scope ctx;
  let then_instructions = List.concat_map (convert_statement_to_blocks ctx) then_stmts in
  let then_final_instructions = match then_final with
    | Some expr ->
      let temp = generate_temp ctx in
      [Assign (temp, expression_to_ir_value ctx expr)]
    | None -> []
  in
  pop_scope ctx;
  let then_block_ir = create_block_with_terminator then_label 
    (then_instructions @ then_final_instructions) (Jump merge_label) in
  add_block ctx then_block_ir;
  
  (* Create else block *)
  let else_block_ir = match else_block with
    | Some (else_stmts, else_final) ->
      push_scope ctx;
      let else_instructions = List.concat_map (convert_statement_to_blocks ctx) else_stmts in
      let else_final_instructions = match else_final with
        | Some expr ->
          let temp = generate_temp ctx in
          [Assign (temp, expression_to_ir_value ctx expr)]
        | None -> []
      in
      pop_scope ctx;
      create_block_with_terminator else_label 
        (else_instructions @ else_final_instructions) (Jump merge_label)
    | None ->
      create_block_with_terminator else_label [] (Jump merge_label)
  in
  add_block ctx else_block_ir;
  
  (* Create merge block *)
  let merge_block = create_empty_block merge_label in
  add_block ctx merge_block;
  
  (* Return the branch instruction for the current block *)
  [Branch (cond_ir, then_label, else_label)]

(* Convert for loop to proper basic blocks *)
and convert_for_to_blocks ctx var iter_expr body =
  let iter_ir = expression_to_ir_value ctx iter_expr in
  let init_label = generate_label ctx "for_init" in
  let cond_label = generate_label ctx "for_cond" in
  let body_label = generate_label ctx "for_body" in
  let update_label = generate_label ctx "for_update" in
  let exit_label = generate_label ctx "for_exit" in
  
  (* Declare loop variable *)
  declare_symbol ctx var (IntType 32);
  
  (* Create init block *)
  let init_instructions = [
    Assign (var, Constant 0); (* Initialize to 0 *)
  ] in
  let init_block = create_block_with_terminator init_label init_instructions (Jump cond_label) in
  add_block ctx init_block;
  
  (* Create condition block *)
  let temp_end = generate_temp ctx in
  let cond_instructions = [
    Assign (temp_end, iter_ir); (* Get end value from range *)
  ] in
  let condition = BinaryOp (Variable var, ILt, Variable temp_end) in
  let cond_block = create_block_with_terminator cond_label cond_instructions 
    (Branch (condition, body_label, exit_label)) in
  add_block ctx cond_block;
  
  (* Create body block *)
  push_scope ctx;
  let old_continue = ctx.continue_label in
  let old_break = ctx.break_label in
  ctx.continue_label <- Some update_label;
  ctx.break_label <- Some exit_label;
  
  let (body_stmts, body_final) = body in
  let body_instructions = List.concat_map (convert_statement_to_blocks ctx) body_stmts in
  let body_final_instructions = match body_final with
    | Some expr ->
      let temp = generate_temp ctx in
      [Assign (temp, expression_to_ir_value ctx expr)]
    | None -> []
  in
  
  ctx.continue_label <- old_continue;
  ctx.break_label <- old_break;
  pop_scope ctx;
  
  let body_block = create_block_with_terminator body_label 
    (body_instructions @ body_final_instructions) (Jump update_label) in
  add_block ctx body_block;
  
  (* Create update block *)
  let increment = BinaryOp (Variable var, IAdd, Constant 1) in
  let update_instructions = [Assign (var, increment)] in
  let update_block = create_block_with_terminator update_label update_instructions (Jump cond_label) in
  add_block ctx update_block;
  
  (* Create exit block *)
  let exit_block = create_empty_block exit_label in
  add_block ctx exit_block;
  
  (* Return jump to init *)
  [Jump init_label]

(* Convert while loop to proper basic blocks *)
and convert_while_to_blocks ctx cond body =
  let cond_label = generate_label ctx "while_cond" in
  let body_label = generate_label ctx "while_body" in
  let exit_label = generate_label ctx "while_exit" in
  
  (* Create condition block *)
  let cond_ir = expression_to_ir_value ctx cond in
  let cond_block = create_block_with_terminator cond_label [] 
    (Branch (cond_ir, body_label, exit_label)) in
  add_block ctx cond_block;
  
  (* Create body block *)
  push_scope ctx;
  let old_continue = ctx.continue_label in
  let old_break = ctx.break_label in
  ctx.continue_label <- Some cond_label;
  ctx.break_label <- Some exit_label;
  
  let (body_stmts, body_final) = body in
  let body_instructions = List.concat_map (convert_statement_to_blocks ctx) body_stmts in
  let body_final_instructions = match body_final with
    | Some expr ->
      let temp = generate_temp ctx in
      [Assign (temp, expression_to_ir_value ctx expr)]
    | None -> []
  in
  
  ctx.continue_label <- old_continue;
  ctx.break_label <- old_break;
  pop_scope ctx;
  
  let body_block = create_block_with_terminator body_label 
    (body_instructions @ body_final_instructions) (Jump cond_label) in
  add_block ctx body_block;
  
  (* Create exit block *)
  let exit_block = create_empty_block exit_label in
  add_block ctx exit_block;
  
  (* Return jump to condition *)
  [Jump cond_label]

(* Convert infinite loop to proper basic blocks *)
and convert_loop_to_blocks ctx body =
  let loop_label = generate_label ctx "loop" in
  
  (* Create loop body block *)
  push_scope ctx;
  let old_continue = ctx.continue_label in
  let old_break = ctx.break_label in
  ctx.continue_label <- Some loop_label;
  (* break_label would need to be set by outer context *)
  
  let (body_stmts, body_final) = body in
  let body_instructions = List.concat_map (convert_statement_to_blocks ctx) body_stmts in
  let body_final_instructions = match body_final with
    | Some expr ->
      let temp = generate_temp ctx in
      [Assign (temp, expression_to_ir_value ctx expr)]
    | None -> []
  in
  
  ctx.continue_label <- old_continue;
  ctx.break_label <- old_break;
  pop_scope ctx;
  
  let loop_block = create_block_with_terminator loop_label 
    (body_instructions @ body_final_instructions) (Jump loop_label) in
  add_block ctx loop_block;
  
  (* Return jump to loop *)
  [Jump loop_label]

(* Convert function to IR with proper basic blocks *)
let convert_function ctx func_def =
  let params = List.map (fun param -> 
    (param.param_name, ast_type_to_ir_type param.param_type)) func_def.func_params in
  let return_type = match func_def.func_return with
    | Some rt -> ast_type_to_ir_type rt
    | None -> VoidType
  in
  
  (* Reset context for this function *)
  ctx.current_blocks <- [];
  ctx.current_function <- Some func_def.func_name;
  push_scope ctx; (* Function scope *)
  
  (* Declare parameters in function scope *)
  List.iter (fun (name, ir_type) -> declare_symbol ctx name ir_type) params;
  
  (* Convert function body *)
  let (statements, final_expr) = func_def.func_body in
  let body_instructions = List.concat_map (convert_statement_to_blocks ctx) statements in
  
  (* Handle final expression *)
  let final_instructions = match final_expr with
    | Some expr ->
      let ir_value = expression_to_ir_value ctx expr in
      [Return (Some ir_value)]
    | None -> [Return None]
  in
  
  (* Create entry block *)
  let entry_block = create_block_with_terminator "entry" 
    (body_instructions @ final_instructions) (Return None) in
  
  (* If we generated additional blocks during control flow, they're already added *)
  let all_blocks = entry_block :: (get_blocks_in_order ctx) in
  
  pop_scope ctx; (* Remove function scope *)
  
  {
    name = func_def.func_name;
    params = params;
    return_type = return_type;
    locals = []; (* TODO: collect locals from scopes *)
    blocks = all_blocks;
  }

(* Convert an AST program to IR module *)
let ast_program_to_ir_module (program : Ast.program) : ir_module =
  let ctx = create_conversion_context () in
  let globals = ref [] in
  let functions = ref [] in
  let structs = ref [] in
  let enums = ref [] in
  
  (* Process each top-level item *)
  let process_item = function
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
        | None -> IntType 32 (* placeholder *)
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
    | _ -> ()
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
