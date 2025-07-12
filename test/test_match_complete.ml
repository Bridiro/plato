let test_match_complete () =
  let tests = [
    "fn main() { match x { } }";                    (* Empty match - should fail *)
    "fn main() { match x { _ => 0 } }";             (* Single arm *)
    "fn main() { match x { _ => 0, y => 1 } }";     (* Multiple arms *)
  ] in
  List.iteri (fun i test ->
    Printf.printf "Complete test %d: %s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_match_complete ()
