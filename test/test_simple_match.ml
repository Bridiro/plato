let test_simple_match () =
  let tests = [
    "match x { _ => 0 }";  (* Just a match expression without surrounding function *)
  ] in
  List.iteri (fun i test ->
    Printf.printf "Simple test %d: %s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.expression lexer_fn lexbuf in  (* Parse as expression, not program *)
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_simple_match ()
