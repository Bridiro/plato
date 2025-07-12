(* Feature Integration Tests *)

let test_complete_programs () =
  let programs = [
    ("fn main() { let x = 42; }", "Simple variable declaration");
    ("fn add(x: int, y: int) -> int { x + y }", "Function with arithmetic");
    ("struct Point { x: int, y: int } fn main() { let p = Point { x: 1, y: 2 }; }", "Struct definition and usage");
    ("enum Color { Red, Green, Blue } fn main() { let c = Color::Red; }", "Enum definition and usage");
    ("fn main() { let arr = [1, 2, 3]; let x = arr[0]; }", "Array creation and indexing");
    ("fn main() { let x = 42; let p = &x; let y = *p; }", "Pointer operations");
    ("fn main() { if true { let x = 1; } else { let x = 2; } }", "Conditional statements");
    ("fn main() { while true { break; } }", "While loop");
    ("fn main() { for i in items { continue; } }", "For loop");
    ("fn main() { loop { break; } }", "Infinite loop");
  ] in
  
  let passed = ref 0 in
  let total = List.length programs in
  
  List.iter (fun (code, desc) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string code in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ %s\n" desc;
      incr passed;
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ %s (Lexer error: %s)\n" desc err
    | Plato.Parser.Error -> Printf.printf "✗ %s (Parser error)\n" desc
    | exn -> Printf.printf "✗ %s (Error: %s)\n" desc (Printexc.to_string exn)
  ) programs;
  
  Printf.printf "\nIntegration Tests: %d/%d passed\n" !passed total;
  !passed = total

let test_complex_features () =
  let complex_tests = [
    ("fn factorial(n: int) -> int { if n <= 1 { 1 } else { n * factorial(n - 1) } }", "Recursive function");
    ("struct Point { x: int, y: int } impl Point { fn new(x: int, y: int) -> Point { Point { x, y } } }", "Struct with impl block");
    ("enum Result { Ok(int), Err(str) } fn test() -> Result { Result::Ok(42) }", "Enum with data variants");
    ("fn main() { let arr = [1, 2, 3]; for i in arr { if i > 2 { break; } } }", "Nested control flow");
  ] in
  
  let passed = ref 0 in
  let total = List.length complex_tests in
  
  List.iter (fun (code, desc) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string code in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ %s\n" desc;
      incr passed;
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ %s (Lexer error: %s)\n" desc err
    | Plato.Parser.Error -> Printf.printf "✗ %s (Parser error)\n" desc
    | exn -> Printf.printf "✗ %s (Error: %s)\n" desc (Printexc.to_string exn)
  ) complex_tests;
  
  Printf.printf "\nComplex Feature Tests: %d/%d passed\n" !passed total;
  !passed = total

let run_integration_tests () =
  Printf.printf "=== INTEGRATION TESTS ===\n";
  Printf.printf "\n--- Complete Programs ---\n";
  let basic_ok = test_complete_programs () in
  Printf.printf "\n--- Complex Features ---\n";
  let complex_ok = test_complex_features () in
  Printf.printf "\n=== INTEGRATION TESTS COMPLETE ===\n";
  
  if basic_ok && complex_ok then (
    Printf.printf "✓ All integration tests passed!\n";
    exit 0
  ) else (
    Printf.printf "✗ Some integration tests failed!\n";
    exit 1
  )

let () = run_integration_tests ()
