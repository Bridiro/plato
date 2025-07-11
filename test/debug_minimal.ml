let test_minimal input =
  Printf.printf "Testing: %s\n" input;
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ SUCCESS: %d items\n" (List.length ast);
  with
  | Plato.Parser.Error ->
      Printf.printf "✗ Parser error\n"
  | e ->
      Printf.printf "✗ Error: %s\n" (Printexc.to_string e)

let () =
  (* Test the absolute minimal cases *)
  test_minimal "fn f() { }";
  test_minimal "fn f() { let x = 1; }";
  test_minimal "fn f() { 1 }";
  
  (* Try to narrow down the issue *)
  Printf.printf "\n--- Testing different expressions ---\n";
  test_minimal "fn f() { true }";
  test_minimal "fn f() { false }";
  test_minimal "fn f() { null }";
  test_minimal "fn f() { () }";
  test_minimal "fn f() { x }";
  test_minimal "fn f() { (42) }";
test_minimal "fn main() -> i32 { let x = 42; x }";
