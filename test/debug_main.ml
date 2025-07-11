let debug_parse input =
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let _ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ %s\n" input;
  with 
  | Plato.Parser.Error -> Printf.printf "✗ Parser error: %s\n" input
  | exn -> Printf.printf "✗ %s: %s\n" input (Printexc.to_string exn)

let () =
  Printf.printf "Testing main function variations:\n";
  debug_parse "fn main() -> int { a }";
  debug_parse "fn main() -> int { if b { return mul(a, 2); } }";
  debug_parse "fn main() -> int { if b { return mul(a, 2); } a }";
  debug_parse "fn main() -> int { if true { return 42; } 0 }";
  debug_parse "fn main() -> int { if true { return 42; } else { 0 } }";
  
  Printf.printf "\nTesting problematic parts:\n";
  debug_parse "fn main() -> int { return mul(a, 2); }";
  debug_parse "fn main() -> int { mul(a, 2) }";
  debug_parse "fn main() -> int { if b { 42 } }";
  debug_parse "fn main() -> int { if b { 42 } else { 0 } }"
