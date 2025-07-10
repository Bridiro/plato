let test_simple_parse () =
  let input = "fn main() { 42 }" in
  Printf.printf "Testing simple function parse (no return type):\n";
  try
    let tokens = Plato.Lexer.tokenize input in
    Printf.printf "Tokens: ";
    List.iter (fun token ->
      match token with
      | Plato.Parser.FN -> Printf.printf "FN "
      | Plato.Parser.IDENTIFIER s -> Printf.printf "ID(%s) " s
      | Plato.Parser.LPAREN -> Printf.printf "( "
      | Plato.Parser.RPAREN -> Printf.printf ") "
      | Plato.Parser.ARROW -> Printf.printf "-> "
      | Plato.Parser.LBRACE -> Printf.printf "{ "
      | Plato.Parser.RBRACE -> Printf.printf "} "
      | Plato.Parser.INTEGER (i, _) -> Printf.printf "INT(%d) " i
      | Plato.Parser.EOF -> Printf.printf "EOF "
      | _ -> Printf.printf "OTHER "
    ) tokens;
    Printf.printf "\n";
    
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ Parsed successfully: %d items\n" (List.length ast);
  with
  | Plato.Lexer.LexError msg ->
      Printf.printf "✗ Lexer error: %s\n" msg
  | Plato.Parser.Error ->
      Printf.printf "✗ Parser error\n"
  | e ->
      Printf.printf "✗ Other error: %s\n" (Printexc.to_string e)

let () = test_simple_parse ()
