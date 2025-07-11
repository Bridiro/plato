let test_features () =
  let tests = [
    (* Loop constructs *)
    "fn main() { loop { break; } }";
    "fn main() { while true { break; } }";
    "fn main() { for i in items { break; } }";
    
    (* Break/continue *)
    "fn main() { loop { break; } }";
    "fn main() { loop { continue; } }";
    "fn main() { loop { break 42; } }";
    
    (* Arrays *)
    "fn main() { let arr = [1, 2, 3]; }";
    "fn main() { let x = arr[0]; }";
    
    (* Structs *)
    "struct Point { x: int, y: int }";
    "fn main() { let p = Point { x: 1, y: 2 }; }";
    
    (* Match expressions *)
    "fn main() { match x { 1 => 42, _ => 0 } }";
    
    (* Pointers *)
    "fn main() { let x = 42; let p = &x; let y = *p; }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Feature %d: " (i + 1);
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_features ()
