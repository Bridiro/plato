let test_match () =
  let tests = [
    "fn main() { match x { _ => 0 } }";
    "fn main() { match x { 1 => 42 } }";
    "fn main() { match x { 1 => 42, _ => 0 } }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Match test %d: %s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_match ()
