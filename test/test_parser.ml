(* Comprehensive Parser Tests *)

let test_success_cases () =
  let success_cases = [
    (* Basic statements in functions *)
    ("fn main() { let x = 42; }", "Variable declaration");
    ("fn main() { let mut x = 42; }", "Mutable variable declaration");
    ("fn main() { x = 10; }", "Assignment");
    
    (* Function definitions *)
    ("fn main() { }", "Empty function");
    ("fn add(x: int, y: int) -> int { x + y }", "Function with parameters and return");
    ("fn test(x: int,) -> int { x }", "Function with trailing comma");
    
    (* Control flow *)
    ("fn main() { if true { let x = 1; } }", "If statement");
    ("fn main() { if true { let x = 1; } else { let x = 2; } }", "If-else statement");
    ("fn main() { while true { break; } }", "While loop");
    ("fn main() { for i in items { continue; } }", "For loop");
    ("fn main() { loop { break; } }", "Infinite loop");
    
    (* Expressions in functions *)
    ("fn main() { let result = x + y * z; }", "Arithmetic expression");
    ("fn main() { let x = arr[0]; }", "Array indexing");
    ("fn main() { let field = obj.field; }", "Field access");
    ("fn main() { func(1, 2, 3); }", "Function call");
    ("fn main() { func(1, 2, 3,); }", "Function call with trailing comma");
    ("fn main() { let arr = [1, 2, 3]; }", "Array literal");
    ("fn main() { let arr = [1, 2, 3,]; }", "Array literal with trailing comma");
    
    (* Struct and enum definitions *)
    ("struct Point { x: int, y: int }", "Struct definition");
    ("struct Point { x: int, y: int, }", "Struct with trailing comma");
    ("enum Color { Red, Green, Blue }", "Enum definition");
    ("enum Color { Red, Green, Blue, }", "Enum with trailing comma");
    ("enum Result { Ok(int), Err(str) }", "Enum with data");
    
    (* Struct expressions *)
    ("fn main() { let p = Point { x: 1, y: 2 }; }", "Struct expression");
    ("fn main() { let p = Point { x: 1, y: 2, }; }", "Struct expression with trailing comma");
    
    (* Type annotations *)
    ("fn main() { let x: int = 42; }", "Type annotation");
    ("fn test(x: *int) { }", "Pointer type parameter");
    ("fn test(x: [int]) { }", "Array type parameter");
    
    (* Complex expressions *)
    ("fn main() { let ptr = &x; }", "Address of");
    ("fn main() { let val = *ptr; }", "Dereference");
    ("fn main() { let val = x as int; }", "Type cast");
  ] in
  
  let passed = ref 0 in
  let total = List.length success_cases in
  
  List.iter (fun (code, desc) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string code in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ %s: %s\n" desc code;
      incr passed;
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ %s: %s (Lexer error: %s)\n" desc code err
    | Plato.Parser.Error -> Printf.printf "✗ %s: %s (Parser error)\n" desc code
    | exn -> Printf.printf "✗ %s: %s (Error: %s)\n" desc code (Printexc.to_string exn)
  ) success_cases;
  
  Printf.printf "\nParser Tests: %d/%d passed\n" !passed total;
  !passed = total

let test_error_cases () =
  let error_cases = [
    ("let x", "Incomplete let statement");
    ("fn main() {", "Unclosed function");
    ("x + ", "Incomplete expression");
    ("struct { }", "Struct without name");
    ("enum { }", "Enum without name");
    ("if { }", "If without condition");
    ("while { }", "While without condition");
    ("for { }", "For without iterator");
  ] in
  
  let failed_correctly = ref 0 in
  let total = List.length error_cases in
  
  List.iter (fun (code, desc) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string code in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✗ %s: %s (Should have failed but didn't)\n" desc code;
    with
    | Plato.Lexer.LexError _ -> 
      Printf.printf "✓ %s: %s (Failed as expected - Lexer)\n" desc code;
      incr failed_correctly;
    | Plato.Parser.Error -> 
      Printf.printf "✓ %s: %s (Failed as expected - Parser)\n" desc code;
      incr failed_correctly;
    | exn -> 
      Printf.printf "✓ %s: %s (Failed as expected - %s)\n" desc code (Printexc.to_string exn);
      incr failed_correctly;
  ) error_cases;
  
  Printf.printf "\nError Tests: %d/%d failed correctly\n" !failed_correctly total;
  !failed_correctly = total

let run_parser_tests () =
  Printf.printf "=== PARSER TESTS ===\n";
  Printf.printf "\n--- Success Cases ---\n";
  let success_ok = test_success_cases () in
  Printf.printf "\n--- Error Cases ---\n";
  let error_ok = test_error_cases () in
  Printf.printf "\n=== PARSER TESTS COMPLETE ===\n";
  
  if success_ok && error_ok then (
    Printf.printf "✓ All parser tests passed!\n";
    exit 0
  ) else (
    Printf.printf "✗ Some parser tests failed!\n";
    exit 1
  )

let () = run_parser_tests ()
