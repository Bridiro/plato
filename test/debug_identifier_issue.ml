let debug_identifier_issue () =
  let tests = [
    "fn main() -> int { if flag { return 2; } else { 1 } }";
    "fn main() -> int { if x { return 2; } else { 1 } }";
    "fn main() -> int { if condition { return 2; } else { 1 } }";
    "fn main() -> int { flag }";
    "fn main() -> int { b }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Test %d: " (i + 1);
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = debug_identifier_issue ()
