let test_type_parsing () =
  let tests = [
    "fn test() -> int { 42 }";    (* Should work - primitive type *)
    "fn test() -> Point { Point { x: 0, y: 0 } }"; (* Should work - user-defined type *)
    "fn test(p: Point) -> int { p.x }"; (* Should work - user-defined type in parameter *)
  ] in
  List.iteri (fun i test ->
    Printf.printf "Type test %d: %s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_type_parsing ()
