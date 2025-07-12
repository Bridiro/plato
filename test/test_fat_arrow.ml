let test_fat_arrow () =
  let tests = [
    (* Test if fat arrow lexing works *)
    "fn main() { let x = 1 => 2; }";  (* This should fail but test if => is recognized *)
    "fn main() { match x { } }";       (* Empty match to test match keyword *)
  ] in
  List.iteri (fun i test ->
    Printf.printf "Arrow test %d: %s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_fat_arrow ()
