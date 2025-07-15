(* Test IR generation *)

let test_ir_generation () =
  let test_cases = [
    ("Simple function", "fn add(x: i32, y: i32) -> i32 { x + y }");
    ("Variable declaration", "fn main() { let x = 42; }");
    ("Array operations", "fn main() { let arr = [1, 2, 3]; arr[0] = 10; }");
    ("Struct creation", "struct Point { x: i32, y: i32 } fn main() { let p = Point { x: 1, y: 2 }; }");
    ("Global constants", "const VALUE: i32 = 100; fn main() { let x = VALUE; }");
  ] in
  
  Printf.printf "\n--- IR Generation Tests ---\n";
  List.iter (fun (name, code) ->
    try
      let lexer_fn, lexbuf = Eiron.Lexer.parse_string code in
      Lexing.set_filename lexbuf "test.eiron";
      let ast = Eiron.Parser.program lexer_fn lexbuf in
      Eiron.Type_checker.type_check_program ast "test.eiron";
      let ir_module = Eiron.Ast_to_ir.convert_ast_to_ir ast in
      Printf.printf "✓ %s\n" name;
      Printf.printf "  IR:\n%s\n" (Eiron.Ir.string_of_ir_module ir_module);
    with
    | Eiron.Error.CompilerError error ->
      Printf.printf "✗ %s: %s\n" name (Eiron.Error.format_error error)
    | exn -> 
      Printf.printf "✗ %s: %s\n" name (Printexc.to_string exn)
  ) test_cases

let () = test_ir_generation ()
