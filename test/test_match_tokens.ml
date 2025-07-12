let test_match_tokens () =
  let text = "match x { _ => 0 }" in
  Printf.printf "Testing tokenization of: %s\n" text;
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string text in
    let rec print_tokens () =
      match lexer_fn lexbuf with
      | Plato.Parser.EOF -> Printf.printf "EOF\n"
      | token -> 
        Printf.printf "%s " (match token with
          | MATCH -> "MATCH"
          | IDENTIFIER s -> "IDENTIFIER(" ^ s ^ ")"
          | LBRACE -> "LBRACE"
          | UNDERSCORE -> "UNDERSCORE"
          | FAT_ARROW -> "FAT_ARROW"
          | INTEGER (i, _) -> "INTEGER(" ^ string_of_int i ^ ")"
          | RBRACE -> "RBRACE"
          | _ -> "OTHER");
        print_tokens ()
    in
    print_tokens ()
  with
  | Plato.Lexer.LexError err -> Printf.printf "Lexer error: %s\n" err
  | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)

let () = test_match_tokens ()
