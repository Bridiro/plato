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
  (* Local variables collection for current function *)
  mutable locals: (string * ir_type) list;
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
  locals = []; (* Start with empty locals *)
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
  | current_scope :: _ -> 
    Hashtbl.replace current_scope name ir_type;
    (* Add to locals if we're in a function and not in global scope *)
    if ctx.current_function <> None && List.length ctx.scopes > 1 then
      ctx.locals <- (name, ir_type) :: ctx.locals

(* Type inference helpers *)
let infer_type_from_literal = function
  | IntLit (_, suffix_opt) ->
    (match suffix_opt with
    | Some I8 -> IntType 8
    | Some I16 -> IntType 16
    | Some I32 -> IntType 32
    | Some I64 -> IntType 64
    | Some U8 -> IntType 8  (* Unsigned -> signed for now *)
    | Some U16 -> IntType 16
    | Some U32 -> IntType 32
    | Some U64 -> IntType 64
    | Some Usize -> IntType 64
    | None -> IntType 32)
  | FloatLit (_, suffix_opt) ->
    (match suffix_opt with
    | Some F32 -> FloatType 32
    | Some F64 -> FloatType 64
    | None -> FloatType 64)
  | StringLit _ -> StringType
  | CharLit _ -> IntType 8
  | BoolLit _ -> BoolType
  | UnitLit -> VoidType
  | NullLit -> PointerType VoidType

let rec infer_type_from_expression ctx = function
  | Ast.Literal (lit, _) -> infer_type_from_literal lit
  | Ast.Identifier (name, _) ->
    (match lookup_symbol ctx name with
    | Some ty -> ty
    | None -> IntType 32) (* Default fallback *)
  | Ast.BinaryOp (left, op, right, _) ->
    let left_type = infer_type_from_expression ctx left in
    let _right_type = infer_type_from_expression ctx right in
    (* For now, use left operand type - proper type unification would be more complex *)
    left_type
  | Ast.UnaryOp (op, expr, _) ->
    infer_type_from_expression ctx expr
  | Ast.Cast (_, target_type, _) ->
    ast_type_to_ir_type target_type
  | Ast.Index (array, _, _) ->
    (* For array access, return element type - simplified *)
    (match infer_type_from_expression ctx array with
    | ArrayType (elem_type, _) -> elem_type
    | _ -> IntType 32)
  | Ast.FunctionCall (_, _, _) ->
    IntType 32 (* Would need function signature lookup *)
  | Ast.Range (_, _, _) ->
    ArrayType (IntType 32, 2) (* Range creates array of 2 integers [start, end] *)
  | _ -> IntType 32 (* Default fallback *)

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
(* Convert statements to instructions, generating proper basic blocks *)
(* Convert expression statements with proper control flow *)
(* Convert if statement to proper basic blocks *)
(* Convert for loop to proper basic blocks *)
(* Convert while loop to proper basic blocks *)
(* Convert infinite loop to proper basic blocks *)
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
    (* For block expressions, evaluate the block and return the final expression value *)
    let (stmts, final_expr) = block in
    (* Process statements but don't emit instructions here - this is for expression context *)
    (match final_expr with
    | Some expr -> expression_to_ir_value ctx expr
    | None -> Constant 0)  (* Unit value *)
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
  | Ast.If (cond, then_block, else_block, _) ->
    (* For if expressions, generate the control flow blocks *)
    let _ = convert_if_to_blocks ctx cond then_block else_block in
    Constant 0  (* Return unit for now - proper implementation needs phi nodes *)
  | Ast.Match (expr, arms, _) ->
    (* Match expressions need complex IR generation - placeholder for now *)
    Constant 0
  | Ast.Loop (body, _) ->
    (* Loop expressions - generate the actual loop blocks *)
    let _ = convert_loop_to_blocks ctx body in
    Constant 0
  | Ast.While (cond, body, _) ->
    (* While expressions - generate the actual while blocks *)
    let _ = convert_while_to_blocks ctx cond body in
    Constant 0
  | Ast.For (var, iter, body, _) ->
    (* For expressions - generate the actual for blocks *)
    let _ = convert_for_to_blocks ctx var iter body in
    Constant 0

(* Convert statements to instructions, generating proper basic blocks *)
and convert_statement_to_blocks ctx stmt =
  match stmt with
  | LetStmt (is_mutable, name, type_opt, init_expr_opt) ->
    (match init_expr_opt with
    | Some init_expr ->
      let ir_value = expression_to_ir_value ctx init_expr in
      let ir_type = match type_opt with
        | Some t -> ast_type_to_ir_type t
        | None -> infer_type_from_expression ctx init_expr (* Proper type inference *)
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
  (* Extract start and end values from range expression *)
  let (start_value, end_value) = match iter_expr with
    | Range (start_expr, end_expr, _) ->
      (expression_to_ir_value ctx start_expr, expression_to_ir_value ctx end_expr)
    | _ ->
      (* For non-range iterators, assume 0..n *)
      let iter_ir = expression_to_ir_value ctx iter_expr in
      (Constant 0, iter_ir)
  in
  
  let init_label = generate_label ctx "for_init" in
  let cond_label = generate_label ctx "for_cond" in
  let body_label = generate_label ctx "for_body" in
  let update_label = generate_label ctx "for_update" in
  let exit_label = generate_label ctx "for_exit" in
  
  (* Declare loop variable *)
  declare_symbol ctx var (IntType 32);
  
  (* Create init block *)
  let init_instructions = [
    Assign (var, start_value); (* Initialize to start value, not 0! *)
  ] in
  let init_block = create_block_with_terminator init_label init_instructions (Jump cond_label) in
  add_block ctx init_block;
  
  (* Create condition block *)
  let temp_end = generate_temp ctx in
  let cond_instructions = [
    Assign (temp_end, end_value); (* Store end value *)
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
  
  (* Create exit block with proper termination for function end *)
  let exit_block = create_block_with_terminator exit_label [] (Return None) in
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
  ctx.locals <- []; (* Reset locals for this function *)
  ctx.current_function <- Some func_def.func_name;
  push_scope ctx; (* Function scope *)
  
  (* Declare parameters in function scope *)
  List.iter (fun (name, ir_type) -> declare_symbol ctx name ir_type) params;
  
  (* Convert function body *)
  let (statements, final_expr) = func_def.func_body in
  
  (* Process statements and collect all generated blocks *)
  let entry_instructions = ref [] in
  let has_control_flow = ref false in
  
  List.iter (fun stmt ->
    let stmt_instructions = convert_statement_to_blocks ctx stmt in
    (* If we got control flow instructions (like Jump), mark that we have control flow *)
    List.iter (function
      | Jump _ | Branch _ -> has_control_flow := true
      | _ -> ()
    ) stmt_instructions;
    entry_instructions := !entry_instructions @ stmt_instructions
  ) statements;
  
  (* Handle final expression - avoid double processing control flow *)
  let final_instructions = match final_expr with
    | Some expr ->
      (match expr with
      | For _ | While _ | Loop _ | If _ when !has_control_flow -> 
        (* Control flow expressions that were processed as statements don't need return *)
        []
      | _ ->
        let ir_value = expression_to_ir_value ctx expr in
        [Return (Some ir_value)])
    | None when !has_control_flow -> 
      (* No final expression and we have control flow - return is handled by exit blocks *)
      []
    | None -> 
      [Return None]
  in
  
  (* Create entry block *)
  let entry_block = if !has_control_flow then
    (* Control flow case - include the control flow instructions but no return *)
    { label = "entry"; instructions = !entry_instructions }
  else
    (* Normal case - include all instructions plus return *)
    let all_entry_instructions = !entry_instructions @ final_instructions in
    { label = "entry"; instructions = all_entry_instructions }
  in
  
  (* Combine entry block with any additional blocks generated during control flow *)
  let additional_blocks = get_blocks_in_order ctx in
  let all_blocks = entry_block :: additional_blocks in
  
  (* Post-process: if we have additional blocks but entry block has return, fix it *)
  let final_blocks = if List.length additional_blocks > 0 then
    (* We have control flow blocks - ensure entry block doesn't have spurious return *)
    let corrected_entry = { label = "entry"; instructions = !entry_instructions } in
    corrected_entry :: additional_blocks
  else
    all_blocks
  in
  
  pop_scope ctx; (* Remove function scope *)
  
  {
    name = func_def.func_name;
    params = params;
    return_type = return_type;
    locals = List.rev ctx.locals; (* Collect locals from context *)
    blocks = final_blocks;
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
        | None -> infer_type_from_expression ctx init_expr (* Proper type inference *)
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
