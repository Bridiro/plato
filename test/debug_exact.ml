let debug_parse input =
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let _ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ %s\n" input;
  with 
  | Plato.Parser.Error -> Printf.printf "✗ Parser error: %s\n" input
  | exn -> Printf.printf "✗ %s: %s\n" input (Printexc.to_string exn)

let test_full_file () =
  let content = "let a: int = 3;\nlet b: bool = true;\n\nfn mul(x: int, y: int) -> int {\n    let r = x * y;\n    r\n}\n\nfn main() -> int {\n    if b {\n        return mul(a, 2);\n    }\n    a\n}\n" in
  debug_parse content

let () =
  Printf.printf "Testing exact content:\n";
  test_full_file ();
  
  Printf.printf "\nTesting with exact main function:\n";
  debug_parse "fn main() -> int {\n    if b {\n        return mul(a, 2);\n    }\n    a\n}";
  
  Printf.printf "\nTesting components:\n";
  debug_parse "if b { return mul(a, 2); }";
  debug_parse "if b { return mul(a, 2); } a";
