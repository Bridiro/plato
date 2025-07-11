let test_for_loop () =
  let tests = [
    "fn main() { for i in [1, 2, 3] { break; } }";
    "fn main() { for x in items { break; } }";
    "fn main() { for item in collection { break; } }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "For test %d: " (i + 1);
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_for_loop ()
