let debug_minimal_if () =
  let tests = [
    "fn main() -> int { if true { 1 } else { 2 } }";
    "fn main() -> int { if false { 1 } else { 2 } }";
    "fn main() -> int { if x { 1 } else { 2 } }";
    "fn main() -> int { true }";
    "fn main() -> int { false }";
    "fn main() -> int { x }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Test %d: %s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = debug_minimal_if ()
