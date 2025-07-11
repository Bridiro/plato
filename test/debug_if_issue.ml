let debug_if_issue () =
  let tests = [
    "fn main() -> int { if true { return 2; } else { 1 } }";
    "fn main() -> int { if b { return 2; } else { 1 } }";
    "fn main() -> int { if b { return mul(a, 2); } else { a } }";
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

let () = debug_if_issue ()
