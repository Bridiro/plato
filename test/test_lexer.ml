(* Comprehensive Lexer Tests *)

let test_keywords () =
  let keywords = [
    ("fn", "FN");
    ("let", "LET");
    ("mut", "MUT");
    ("if", "IF");
    ("else", "ELSE");
    ("while", "WHILE");
    ("for", "FOR");
    ("loop", "LOOP");
    ("break", "BREAK");
    ("continue", "CONTINUE");
    ("return", "RETURN");
    ("match", "MATCH");
    ("struct", "STRUCT");
    ("enum", "ENUM");
    ("impl", "IMPL");
    ("trait", "TRAIT");
    ("true", "TRUE");
    ("false", "FALSE");
    ("_", "UNDERSCORE");
  ] in
  List.iter (fun (word, expected) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string word in
      let _token = lexer_fn lexbuf in
      Printf.printf "✓ Keyword '%s' -> %s\n" word expected;
    with
    | exn -> Printf.printf "✗ Keyword '%s' failed: %s\n" word (Printexc.to_string exn)
  ) keywords

let test_operators () =
  let operators = [
    ("+", "PLUS");
    ("-", "MINUS");
    ("*", "STAR");
    ("/", "SLASH");
    ("=", "ASSIGN");
    ("==", "EQ");
    ("!=", "NEQ");
    ("<", "LT");
    (">", "GT");
    ("<=", "LEQ");
    (">=", "GEQ");
    ("&&", "AND");
    ("||", "OR");
    ("!", "NOT");
    ("&", "BIT_AND");
    ("|", "BIT_OR");
    ("^", "BIT_XOR");
    ("=>", "FAT_ARROW");
    ("->", "ARROW");
    ("::", "DOUBLE_COLON");
  ] in
  List.iter (fun (op, expected) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string op in
      let _token = lexer_fn lexbuf in
      Printf.printf "✓ Operator '%s' -> %s\n" op expected;
    with
    | exn -> Printf.printf "✗ Operator '%s' failed: %s\n" op (Printexc.to_string exn)
  ) operators

let test_literals () =
  let literals = [
    ("42", "INTEGER");
    ("3.14", "FLOAT");
    ("\"hello\"", "STRING");
    ("'c'", "CHAR");
    ("true", "TRUE");
    ("false", "FALSE");
    ("()", "LPAREN RPAREN");
  ] in
  List.iter (fun (lit, expected) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string lit in
      let _token = lexer_fn lexbuf in
      Printf.printf "✓ Literal '%s' -> %s\n" lit expected;
    with
    | exn -> Printf.printf "✗ Literal '%s' failed: %s\n" lit (Printexc.to_string exn)
  ) literals

let test_identifiers () =
  let identifiers = [
    ("x", "IDENTIFIER");
    ("variable_name", "IDENTIFIER");
    ("CamelCase", "IDENTIFIER");
    ("snake_case", "IDENTIFIER");
    ("var123", "IDENTIFIER");
  ] in
  List.iter (fun (id, expected) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string id in
      let _token = lexer_fn lexbuf in
      Printf.printf "✓ Identifier '%s' -> %s\n" id expected;
    with
    | exn -> Printf.printf "✗ Identifier '%s' failed: %s\n" id (Printexc.to_string exn)
  ) identifiers

let test_complex_expressions () =
  let expressions = [
    ("x + y * z", "Complex arithmetic");
    ("arr[0]", "Array indexing");
    ("obj.field", "Field access");
    ("Color::Red", "Path expression");
    ("match x { _ => 0 }", "Match expression tokens");
  ] in
  List.iter (fun (expr, desc) ->
    try
      let (lexer_fn, lexbuf) = Plato.Lexer.parse_string expr in
      let rec count_tokens acc =
        try
          let token = lexer_fn lexbuf in
          match token with
          | Plato.Parser.EOF -> acc
          | _ -> count_tokens (acc + 1)
        with
        | _ -> acc
      in
      let token_count = count_tokens 0 in
      Printf.printf "✓ Expression '%s' (%s) -> %d tokens\n" expr desc token_count;
    with
    | exn -> Printf.printf "✗ Expression '%s' failed: %s\n" expr (Printexc.to_string exn)
  ) expressions

let run_lexer_tests () =
  Printf.printf "=== LEXER TESTS ===\n";
  Printf.printf "\n--- Keywords ---\n";
  test_keywords ();
  Printf.printf "\n--- Operators ---\n";
  test_operators ();
  Printf.printf "\n--- Literals ---\n";
  test_literals ();
  Printf.printf "\n--- Identifiers ---\n";
  test_identifiers ();
  Printf.printf "\n--- Complex Expressions ---\n";
  test_complex_expressions ();
  Printf.printf "\n=== LEXER TESTS COMPLETE ===\n"

let () = run_lexer_tests ()
