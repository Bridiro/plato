let test_trailing_commas () =
  let tests = [
    (* Struct definitions *)
    "struct Point { x: int, y: int }";          (* No trailing comma *)
    "struct Point { x: int, y: int, }";         (* With trailing comma *)
    
    (* Enum definitions *)
    "enum Color { Red, Green, Blue }";          (* No trailing comma *)
    "enum Color { Red, Green, Blue, }";         (* With trailing comma *)
    
    (* Struct expressions *)
    "fn main() { let p = Point { x: 1, y: 2 }; }";     (* No trailing comma *)
    "fn main() { let p = Point { x: 1, y: 2, }; }";    (* With trailing comma *)
    
    (* Arrays *)
    "fn main() { let arr = [1, 2, 3]; }";       (* No trailing comma *)
    "fn main() { let arr = [1, 2, 3,]; }";      (* With trailing comma *)
    
    (* Function calls *)
    "fn main() { foo(1, 2, 3); }";              (* No trailing comma *)
    "fn main() { foo(1, 2, 3,); }";             (* With trailing comma *)
    
    (* Function parameters *)
    "fn test(x: int, y: int) { }";              (* No trailing comma *)
    "fn test(x: int, y: int,) { }";             (* With trailing comma *)
    
    (* Single element cases *)
    "struct Point { x: int, }";                 (* Single field with comma *)
    "enum Status { Ok, }";                      (* Single variant with comma *)
    "fn main() { let arr = [42,]; }";           (* Single element array *)
  ] in
  List.iteri (fun i test ->
    Printf.printf "Trailing test %d: %s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_trailing_commas ()
