let debug_various_identifiers () =
  let tests = [
    "fn main() -> int { if a { 1 } else { 2 } }";
    "fn main() -> int { if b { 1 } else { 2 } }";
    "fn main() -> int { if c { 1 } else { 2 } }";
    "fn main() -> int { if flag { 1 } else { 2 } }";
    "fn main() -> int { if condition { 1 } else { 2 } }";
    "fn main() -> int { if var { 1 } else { 2 } }";
    "fn main() -> int { if test { 1 } else { 2 } }";
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

let () = debug_various_identifiers ()
