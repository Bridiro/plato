let test_conditional_identifiers () =
  let tests = [
    (* Basic identifier tests *)
    "fn main() { x }";
    "fn main() { condition }";
    
    (* If with literals vs identifiers *)
    "fn main() { if true { 1 } }";
    "fn main() { if false { 1 } }";
    "fn main() { if x { 1 } }";
    "fn main() { if condition { 1 } }";
    
    (* If with else *)
    "fn main() { if true { 1 } else { 2 } }";
    "fn main() { if x { 1 } else { 2 } }";
    
    (* While loops *)
    "fn main() { while true { break; } }";
    "fn main() { while x { break; } }";
    
    (* For loops *)
    "fn main() { for i in [1] { break; } }";
    "fn main() { for i in items { break; } }";
    
    (* More complex expressions *)
    "fn main() { if (x) { 1 } }";
    "fn main() { if x == true { 1 } }";
  ] in
  List.iteri (fun i test ->
    Printf.printf "Test %2d: %-40s -> " (i + 1) (String.sub test 0 (min 40 (String.length test)));
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string test in
      let _ast = Plato.Parser.program lexer_fn lexbuf in
      Printf.printf "✓ Success\n";
    with
    | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer: %s\n" err
    | Plato.Parser.Error -> Printf.printf "✗ Parser error\n"
    | exn -> Printf.printf "✗ %s\n" (Printexc.to_string exn)
  ) tests

let () = test_conditional_identifiers ()
