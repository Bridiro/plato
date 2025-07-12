let test_comprehensive_features () =
  let tests = [
    (* Basic constructs - should all work *)
    "fn main() { let x = 42; }";
    "fn main() { loop { break; } }";
    "fn main() { while true { break; } }";
    "fn main() { for i in items { break; } }";
    
    (* Arrays - should work *)
    "fn main() { let arr = [1, 2, 3]; }";
    "fn main() { let x = arr[0]; }";
    
    (* Structs - should work *)
    "struct Point { x: int, y: int }";
    "fn main() { let p = Point { x: 1, y: 2 }; }";
    "fn main() { let p = Point { x: 1, y: 2 }; let x = p.x; }";
    
    (* Enums - let's test if they work *)
    "enum Color { Red, Green, Blue }";
    "enum Result { Ok(int), Err(str) }";
    "fn main() { let c = Color::Red; }";
    
    (* Match expressions - currently failing *)
    "fn main() { match x { 1 => 42, _ => 0 } }";
    "fn main() { match color { Color::Red => 1, Color::Green => 2, Color::Blue => 3 } }";
    
    (* Pointers - should work *)
    "fn main() { let x = 42; let p = &x; let y = *p; }";
    
    (* Impl blocks - let's test *)
    "struct Point { x: int, y: int } impl Point { fn new(x: int, y: int) -> Point { Point { x, y } } }";
    
    (* Traits - let's test *)
    "trait Display { fn display(self: *Point) -> str; }";
    
    (* More complex combinations *)
    "fn main() { let arr = [1, 2, 3]; for i in arr { if i > 2 { break; } } }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Feature %2d: %-80s -> " (i + 1) test;
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_comprehensive_features ()
