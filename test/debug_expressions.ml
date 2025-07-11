let debug_verbose input =
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let _ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ %s\n" input;
  with 
  | Plato.Parser.Error -> 
    Printf.printf "✗ Parser error: %s\n" input;
  | exn -> Printf.printf "✗ %s: %s\n" input (Printexc.to_string exn)

let () =
  Printf.printf "Testing simple expressions:\n";
  debug_verbose "fn main() { mul(1, 2) }";
  debug_verbose "fn main() { x * y }";
  debug_verbose "fn main() { if true { 42 } }";
  debug_verbose "fn main() { return 42; }";
  
  Printf.printf "\nTesting as top level:\n";
  debug_verbose "mul(1, 2)";
  debug_verbose "x * y";
  debug_verbose "if true { 42 }";
  debug_verbose "return 42;"
