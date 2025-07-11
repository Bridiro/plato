let debug_return_issue () =
  let tests = [
    (* Working case with else *)
    "fn main() -> int { if true { return 1; } else { 2 } }";
    
    (* Failing case without else - direct return *)
    "fn main() -> int { if true { return 1; } 2 }";
    
    (* Another failing case *)
    "fn main() -> int { if true { 1 } 2 }";
    
    (* Simple case *)
    "fn main() -> int { let a = 3; a }";
    
    (* Case with if but no else *)
    "fn main() -> int { let a = 3; if true { return 1; } a }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Test %2d: %-50s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = debug_return_issue ()
