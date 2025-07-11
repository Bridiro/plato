let test_file file =
  try
    let content = 
      let ic = open_in file in
      let rec read_all acc =
        try
          let line = input_line ic in
          read_all (acc ^ line ^ "\n")
        with End_of_file ->
          close_in ic;
          acc
      in
      read_all ""
    in
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string content in
    let _ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ Successfully parsed %s\n" file;
  with 
  | Plato.Lexer.LexError err -> Printf.printf "✗ Lexer error in %s: %s\n" file err
  | Plato.Parser.Error -> Printf.printf "✗ Parser error in %s\n" file
  | exn -> Printf.printf "✗ %s: %s\n" file (Printexc.to_string exn)

let () =
  test_file "test/test.plato"
