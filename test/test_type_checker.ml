(* Comprehensive Type Checker Tests *)

(* Simple string contains function *)
let string_contains s sub =
  let len = String.length sub in
  let rec check i =
    if i + len > String.length s then
      false
    else if String.sub s i len = sub then
      true
    else
      check (i + 1)
  in
  check 0

let test_success_cases () =
  let test_cases =
    [
      ("Simple variable declaration", "fn main() { let x = 42; }");
      ("String variable", "fn main() { let s = \"hello\"; }");
      ("Boolean variable", "fn main() { let b = true; }");
      ("Arithmetic expression", "fn main() { let x = 10 + 20; }");
      ("Function call", "fn main() { print(\"hello\"); }");
      ("Variable usage", "fn main() { let x = 42; let y = x + 10; }");
      ( "Multiple variables",
        "fn main() { let a = 1; let b = 2; let c = a + b; }" );
      ( "Built-in functions",
        "fn main() { let s = int_to_string(42); print(s); }" );
      ("Return statement", "fn main() { return 42; }");
      ("If expression", "fn main() { let x = if true { 1 } else { 2 }; }");
    ]
  in

  Printf.printf "\n--- Type Checker Success Cases ---\n" ;
  List.iter
    (fun (name, code) ->
      try
        let lexer_fn, lexbuf = Eiron.Lexer.parse_string code in
        Lexing.set_filename lexbuf "test.eiron" ;
        let ast = Eiron.Parser.program lexer_fn lexbuf in
        Eiron.Type_checker.type_check_program ast "test.eiron" ;
        Printf.printf "✓ %s\n" name
      with
      | Eiron.Error.CompilerError error ->
        Printf.printf "✗ %s: %s\n" name (Eiron.Error.format_error error)
      | exn -> Printf.printf "✗ %s: %s\n" name (Printexc.to_string exn))
    test_cases

let test_error_cases () =
  let test_cases =
    [
      ( "Undefined variable",
        "fn main() { let x = undefined_var; }",
        "Undefined variable" );
      ( "Type mismatch in arithmetic",
        "fn main() { let x = true + 10; }",
        "Type mismatch in binary operation" );
      ( "Undefined function",
        "fn main() { unknown_func(); }",
        "Undefined function" );
      ( "Wrong argument type",
        "fn main() { print(42); }",
        "expects different argument types" );
      ( "Wrong number of arguments",
        "fn main() { print(\"hello\", \"world\"); }",
        "wrong number of arguments" );
      ( "Boolean in arithmetic",
        "fn main() { let x = false * 5; }",
        "Type mismatch in binary operation" );
      ( "String in arithmetic",
        "fn main() { let x = \"hello\" + 5; }",
        "Type mismatch in binary operation" );
      ( "If condition not boolean",
        "fn main() { if 42 { let x = 1; } }",
        "If condition must be boolean" );
    ]
  in

  Printf.printf "\n--- Type Checker Error Cases ---\n" ;
  List.iter
    (fun (name, code, expected_error) ->
      try
        let lexer_fn, lexbuf = Eiron.Lexer.parse_string code in
        Lexing.set_filename lexbuf "test.eiron" ;
        let ast = Eiron.Parser.program lexer_fn lexbuf in
        Eiron.Type_checker.type_check_program ast "test.eiron" ;
        Printf.printf "✗ %s: Expected error but got success\n" name
      with
      | Eiron.Error.CompilerError error ->
        if string_contains (Eiron.Error.format_error error) expected_error then
          Printf.printf "✓ %s (Failed as expected - Type)\n" name
        else
          Printf.printf "✗ %s: Got wrong error: %s\n" name
            (Eiron.Error.format_error error)
      | exn ->
        let error_msg = Printexc.to_string exn in
        if string_contains error_msg expected_error then
          Printf.printf "✓ %s (Failed as expected - Type)\n" name
        else
          Printf.printf "✗ %s: Got unexpected error: %s\n" name error_msg)
    test_cases

let test_complex_programs () =
  let test_cases =
    [
      ( "Function with parameters",
        "fn add(x: int, y: int) -> int { return x + y; } fn main() { let \
         result = add(10, 20); }" );
      ( "Multiple functions",
        "fn helper() -> int { return 42; } fn main() { let x = helper(); }" );
      ( "Global variable",
        "let global_var = 100; fn main() { let x = global_var; }" );
    ]
  in

  Printf.printf "\n--- Complex Type Checking Programs ---\n" ;
  List.iter
    (fun (name, code) ->
      try
        let lexer_fn, lexbuf = Eiron.Lexer.parse_string code in
        Lexing.set_filename lexbuf "test.eiron" ;
        let ast = Eiron.Parser.program lexer_fn lexbuf in
        Eiron.Type_checker.type_check_program ast "test.eiron" ;
        Printf.printf "✓ %s\n" name
      with
      | Eiron.Error.CompilerError error ->
        Printf.printf "✗ %s: %s\n" name (Eiron.Error.format_error error)
      | exn -> Printf.printf "✗ %s: %s\n" name (Printexc.to_string exn))
    test_cases

let run_tests () =
  Printf.printf "=== TYPE CHECKER TESTS ===\n" ;
  test_success_cases () ;
  test_error_cases () ;
  test_complex_programs () ;
  Printf.printf "\n=== TYPE CHECKER TESTS COMPLETE ===\n"

let () = run_tests ()
