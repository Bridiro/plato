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

let debug_semicolon () =
  let tests = [
    (* These should work - expressions with semicolons become statements *)
    "fn main() -> int { if true { return 1; }; 2 }";
    "fn main() -> int { let a = 3; if true { return 1; }; a }";
    
    (* These should fail - expressions without semicolons *)
    "fn main() -> int { if true { return 1; } 2 }";
    "fn main() -> int { let a = 3; if true { return 1; } a }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Test %2d: %-50s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = 
  Printf.printf "Semicolon test:\n";
  debug_semicolon ();
  
  Printf.printf "\nOriginal tests:\n";
  debug_parse "fn main() -> int { if b { return mul(a, 2); }; a }";
  debug_parse "fn main() -> int { if b { return mul(a, 2); } else { 0 }; a }";
  debug_parse "fn main() -> int { if b { return mul(a, 2); } else { a } }";
  
  Printf.printf "\nTesting full corrected program:\n";
  debug_parse "let a: int = 3;\nlet b: bool = true;\nfn mul(x: int, y: int) -> int { let r = x * y; r }\nfn main() -> int { if b { return mul(a, 2); }; a }"
