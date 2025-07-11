let debug_parse input =
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let _ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ %s\n" input;
  with 
  | Plato.Parser.Error -> Printf.printf "✗ Parser error: %s\n" input
  | exn -> Printf.printf "✗ %s: %s\n" input (Printexc.to_string exn)

let () =
  Printf.printf "Testing semicolon fix:\n";
  debug_parse "fn main() -> int { if b { return mul(a, 2); }; a }";
  debug_parse "fn main() -> int { if b { return mul(a, 2); } else { 0 }; a }";
  debug_parse "fn main() -> int { if b { return mul(a, 2); } else { a } }";
  
  Printf.printf "\nTesting full corrected program:\n";
  debug_parse "let a: int = 3;\nlet b: bool = true;\nfn mul(x: int, y: int) -> int { let r = x * y; r }\nfn main() -> int { if b { return mul(a, 2); }; a }"
