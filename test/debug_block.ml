let debug_parse input =
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let _ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ %s\n" input;
  with 
  | Plato.Parser.Error -> Printf.printf "✗ Parser error: %s\n" input
  | exn -> Printf.printf "✗ %s: %s\n" input (Printexc.to_string exn)

let () =
  Printf.printf "Testing block variants:\n";
  debug_parse "fn main() { 42 }";
  debug_parse "fn main() { let x = 42; }"; 
  debug_parse "fn main() { x }";
  debug_parse "fn main() { let x = 42; x }";
  
  Printf.printf "\nTesting with semicolon after expression:\n";
  debug_parse "fn main() { let x = 42; x; }";
  debug_parse "fn main() { let x = 42; 42; }";
  
  Printf.printf "\nTesting simpler cases:\n";
  debug_parse "fn main() { let x = 42; let y = x; }";
  debug_parse "fn main() { let x = 42; return x; }";
