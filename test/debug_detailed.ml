let debug_parse input description =
  Printf.printf "=== Debugging: %s ===\n" description;
  Printf.printf "Input: %s\n" (String.escaped input);
  
  try
    (* First, test lexing *)
    let tokens = Plato.Lexer.tokenize input in
    Printf.printf "Tokens: ";
    List.iter (fun token ->
      let token_str = match token with
        | Plato.Parser.FN -> "FN"
        | Plato.Parser.IDENTIFIER s -> Printf.sprintf "ID(%s)" s
        | Plato.Parser.LPAREN -> "("
        | Plato.Parser.RPAREN -> ")"
        | Plato.Parser.ARROW -> "->"
        | Plato.Parser.LBRACE -> "{"
        | Plato.Parser.RBRACE -> "}"
        | Plato.Parser.COLON -> ":"
        | Plato.Parser.SEMICOLON -> ";"
        | Plato.Parser.INTEGER (i, _) -> Printf.sprintf "INT(%d)" i
        | Plato.Parser.LET -> "LET"
        | Plato.Parser.ASSIGN -> "="
        | Plato.Parser.IF -> "IF"
        | Plato.Parser.ELSE -> "ELSE"
        | Plato.Parser.LE -> "<="
        | Plato.Parser.STAR -> "*"
        | Plato.Parser.MINUS -> "-"
        | Plato.Parser.EOF -> "EOF"
        | _ -> "OTHER"
      in
      Printf.printf "%s " token_str
    ) tokens;
    Printf.printf "\n";
    
    (* Now test parsing *)
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ Parsed successfully: %d items\n" (List.length ast);
    
  with
  | Plato.Lexer.LexError msg ->
      Printf.printf "✗ Lexer error: %s\n" msg
  | Plato.Parser.Error ->
      Printf.printf "✗ Parser error (no details available)\n"
  | e ->
      Printf.printf "✗ Other error: %s\n" (Printexc.to_string e);
      Printf.printf "Backtrace:\n%s\n" (Printexc.get_backtrace ())

let () =
  Printexc.record_backtrace true;
  
  (* Test progressively more complex cases *)
  debug_parse "struct Empty {}" "working case - empty struct";
  debug_parse "fn main() {}" "working case - empty function";
  debug_parse "fn main() { let x = 42; }" "working case - let statement";
  Printf.printf "\n--- Now testing failing cases ---\n";
  debug_parse "fn main() { 42 }" "FAILING: expression in block";
  debug_parse "fn main() { let x = 42; x }" "FAILING: let + expression";
