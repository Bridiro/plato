let test_path_expr () =
  let tests = [
    "fn main() { let x = Color::Red; }";
    "fn main() { let x = std::collections::HashMap::new(); }";
    "fn main() { let x = MyModule::MyStruct::new(); }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Path test %d: %s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_path_expr ()
