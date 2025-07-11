let test_rust_style_if () =
  let tests = [
    (* IF statements (without else) - treated as statements *)
    "fn main() -> int { if true { return 1; } 42 }";
    "fn main() -> int { let x = 5; if x > 3 { return x; } 0 }";
    
    (* IF expressions (with else) - treated as expressions *)
    "fn main() -> int { if true { 1 } else { 2 } }";
    "fn main() -> int { let x = if true { 1 } else { 2 }; x }";
    "fn main() -> int { if false { 1 } else { if true { 2 } else { 3 } } }";
    
    (* Mixed usage *)
    "fn main() -> int { if true { return 1; } if false { 2 } else { 3 } }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Test %2d: %-60s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_rust_style_if ()
