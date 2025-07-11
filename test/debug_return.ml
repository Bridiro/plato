let () =
  let test input = 
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ %s\n" input;
    with _ -> Printf.printf "✗ %s\n" input
  in
  
  test "fn main() {}";
  test "fn main() -> i32 {}";
  test "fn main() { 42 }";
  test "fn main() -> i32 { 42 }";
  test "fn main() { let x = 42; }";
  test "fn main() -> i32 { let x = 42; }";
  test "fn main() { let x = 42; x }";
  test "fn main() -> i32 { let x = 42; x }";
