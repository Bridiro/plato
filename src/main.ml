open Plato

let usage_msg = "plato <file>"
let input_file = ref ""

let set_input_file filename = input_file := filename

let spec_list = []

(* Helper function to read file content and split into lines *)
let read_file_with_lines filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let lines = String.split_on_char '\n' content in
  (content, Array.of_list lines)

(* Uniform error reporting *)
let report_error filename line column message error_type =
  Printf.eprintf "%s error at %s:%d:%d: %s\n" error_type filename line column message

(* Helper function to parse with better error reporting *)
let parse_with_error_reporting content filename =
  let (lexer_fn, lexbuf) = Lexer.parse_string content in
  (* Set the filename in lexbuf for better error reporting *)
  Lexing.set_filename lexbuf filename;
  try
    Parser.program lexer_fn lexbuf
  with
  | Parser.Error -> 
      (* Use our custom position tracking for accurate error reporting *)
      let (line, column, _pos) = Lexer.get_last_token_position () in
      report_error filename line column "Syntax error" "Parser";
      exit 1

let () =
  Arg.parse spec_list set_input_file usage_msg;
  
  if !input_file = "" then begin
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  end;
  
  try
    (* Read the input file *)
    let (content, _source_lines) = read_file_with_lines !input_file in
    
    (* Parse the program *)
    let ast = parse_with_error_reporting content !input_file in
    
    (* For now, just print success and show the AST structure *)
    Printf.printf "✓ Successfully parsed: %s\n" !input_file;
    Printf.printf "Program contains %d top-level items\n" (List.length ast);
    
  with
  | Sys_error msg -> 
      Printf.eprintf "File error: %s\n" msg;
      exit 1
  | Lexer.LexErrorWithPos (msg, line, column) ->
      report_error !input_file line column msg "Lexer";
      exit 1
  | Lexer.LexError err -> 
      Printf.eprintf "Lexer error: %s\n" err;
      exit 1
  | Error.CompilerError error ->
      let (_content, source_lines) = read_file_with_lines !input_file in
      Printf.eprintf "%s\n" (Error.show_error_context error source_lines);
      exit 1
  | exn -> 
      Printf.eprintf "Internal error: %s\n" (Printexc.to_string exn);
      exit 1

