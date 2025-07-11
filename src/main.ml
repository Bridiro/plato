open Plato

let usage_msg = "plato <file>"
let input_file = ref ""

let set_input_file filename = input_file := filename

let spec_list = []

let () =
  Arg.parse spec_list set_input_file usage_msg;
  
  if !input_file = "" then begin
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  end;
  
  try
    (* Read the input file *)
    let ic = open_in !input_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    
    (* Parse the program *)
    let (lexer_fn, lexbuf) = Lexer.parse_string content in
    let ast = Parser.program lexer_fn lexbuf in
    
    (* For now, just print success and show the AST structure *)
    Printf.printf "✓ Successfully parsed: %s\n" !input_file;
    Printf.printf "Program contains %d top-level items\n" (List.length ast);
    
  with
  | Sys_error msg -> 
      Printf.eprintf "Error reading file: %s\n" msg;
      exit 1
  | Lexer.LexError err -> 
      Printf.eprintf "Lexer error: %s\n" err;
      exit 1
  | Parser.Error -> 
      Printf.eprintf "Parser error in file: %s\n" !input_file;
      exit 1
  | exn -> 
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      exit 1

