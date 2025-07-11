let debug_parse input =
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let _ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ %s\n" input;
  with 
  | Plato.Parser.Error -> Printf.printf "✗ Parser error: %s\n" input
  | exn -> Printf.printf "✗ %s: %s\n" input (Printexc.to_string exn)

let () =
  Printf.printf "Testing if without else:\n";
  debug_parse "fn main() { if true { 42 } }";
  debug_parse "fn main() { if true { 42 } else { 0 } }";
  debug_parse "fn main() { if true { println(); } }";
  debug_parse "fn main() { if true { println(); } else { 0 } }";
  debug_parse "fn main() { if true { return; } }";
  debug_parse "fn main() { if true { return; } else { 0 } }";
  debug_parse "fn main() { if true { return 42; } }"
