open Eiron

let usage_msg = "eironc [options] <file>"
let input_file = ref ""
let emit_ir = ref false
let emit_llvm = ref false
let emit_asm = ref false
let output_file = ref ""

let set_input_file filename = input_file := filename

let spec_list = [
  ("--emit-ir", Arg.Set emit_ir, " Generate IR (.ir) file");
  ("-ir", Arg.Set emit_ir, " Generate IR (.ir) file");
  ("--emit-llvm", Arg.Set emit_llvm, " Generate LLVM IR (.ll) file");
  ("-ll", Arg.Set emit_llvm, " Generate LLVM IR (.ll) file");
  ("--emit-asm", Arg.Set emit_asm, " Generate assembly (.s) file");
  ("-s", Arg.Set emit_asm, " Generate assembly (.s) file");
  ("--output", Arg.Set_string output_file, " Set output filename");
  ("-o", Arg.Set_string output_file, " Set output filename");
]

(* Helper function to read file content and split into lines *)
let read_file_with_lines filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic ;
  let lines = String.split_on_char '\n' content in
  (content, Array.of_list lines)

(* Helper function to parse with better error reporting *)
let parse_with_error_reporting content filename =
  let lexer_fn, lexbuf = Lexer.parse_string content in
  (* Set the filename in lexbuf for better error reporting *)
  Lexing.set_filename lexbuf filename ;
  try Parser.program lexer_fn lexbuf
  with Parser.Error ->
    (* Use our custom position tracking for accurate error reporting *)
    let line, column, _pos = Lexer.get_last_token_position () in
    let pos = Error.make_position line column 0 in
    let span = Error.make_span pos pos (Some filename) in
    let error =
      Error.make_error (Error.ParseError "Syntax error") span "Syntax error"
    in
    raise (Error.CompilerError error)

let () =
  Arg.parse spec_list set_input_file usage_msg ;

  if !input_file = "" then begin
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0) ;
    exit 1
  end ;

  try
    (* Read the input file *)
    let content, _source_lines = read_file_with_lines !input_file in

    (* Parse the program *)
    let ast = parse_with_error_reporting content !input_file in

    (* Type check the program and get symbol table *)
    let symbol_table = Eiron.Type_checker.type_check_program_with_symbols ast !input_file in

    (* Convert AST to IR using the symbol table *)
    let ir_module = Eiron.Ast_to_ir.convert_ast_to_ir_with_symbols ast symbol_table in

    (* Set filename context for LLVM generation error reporting *)
    Eiron.Llvm_gen.set_llvm_context_filename !input_file ;

    (* Generate LLVM IR *)
    let llvm_ir = Eiron.Llvm_gen.generate_llvm ir_module in

    (* Determine base output filename *)
    let base_name = 
      if !output_file <> "" then
        !output_file
      else if String.ends_with ~suffix:".eiron" !input_file then
        String.sub !input_file 0 (String.length !input_file - 6)
      else
        !input_file
    in

    let ir_file = base_name ^ ".ir" in
    let ll_file = base_name ^ ".ll" in
    let s_file = base_name ^ ".s" in
    let executable = base_name in

    if !emit_ir then begin
      Printf.printf "Generated IR: %s\n" ir_file ;
      let oc = open_out ir_file in
      output_string oc (Eiron.Ir.string_of_ir_module ir_module) ;
      close_out oc ;
    end ;

    (* Always write LLVM IR to temporary file for compilation *)
    let oc = open_out ll_file in
    output_string oc llvm_ir ;
    close_out oc ;

    (* Compile LLVM IR to assembly *)
    let llc_cmd = Printf.sprintf "llc %s -o %s" ll_file s_file in
    let llc_exit = Sys.command llc_cmd in
    if llc_exit <> 0 then begin
      Printf.eprintf "Error: llc failed with exit code %d\n" llc_exit ;
      exit 1
    end ;

    (* Compile assembly to executable *)
    let clang_cmd = Printf.sprintf "clang %s -o %s" s_file executable in
    let clang_exit = Sys.command clang_cmd in
    if clang_exit <> 0 then begin
      Printf.eprintf "Error: clang failed with exit code %d\n" clang_exit ;
      exit 1
    end ;

    (* Clean up intermediate files unless requested to keep them *)
    if not !emit_llvm then
      Sys.remove ll_file ;
    if not !emit_asm then
      Sys.remove s_file ;

    (* Report what was generated *)
    Printf.printf "✓ Successfully compiled: %s\n" !input_file ;
    Printf.printf "Executable: %s\n" executable ;
    
    if !emit_llvm then
      Printf.printf "Generated LLVM IR: %s\n" ll_file ;
    if !emit_asm then
      Printf.printf "Generated assembly: %s\n" s_file
  with
  | Sys_error msg ->
    Printf.eprintf "File error: %s\n" msg ;
    exit 1
  | Lexer.LexError (msg, line, column) ->
    let pos = Error.make_position line column 0 in
    let span = Error.make_span pos pos (Some !input_file) in
    let error = Error.make_error (Error.LexError msg) span msg in
    let _content, source_lines = read_file_with_lines !input_file in
    Printf.eprintf "%s\n" (Error.show_error_context error source_lines) ;
    exit 1
  | Error.CompilerError error ->
    let _content, source_lines = read_file_with_lines !input_file in
    Printf.eprintf "%s\n" (Error.show_error_context error source_lines) ;
    exit 1
  | exn ->
    Printf.eprintf "Internal error: %s\n" (Printexc.to_string exn) ;
    exit 1
