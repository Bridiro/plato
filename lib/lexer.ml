open Token

type lexer_state = {
  input : string;
  length : int;
  mutable pos : int;
  mutable line : int;
  mutable column : int;
}

let init_lexer input =
    {
      input;
      length = String.length input;
      pos = 0;
      line = 1;
      column = 1;
    }

let peek state =
    if state.pos < state.length then
      Some state.input.[state.pos]
    else
      None

let advance state =
    match peek state with
        | Some '\n' ->
            state.pos <- state.pos + 1;
            state.line <- state.line + 1;
            state.column <- 1
        | Some _ ->
            state.pos <- state.pos + 1;
            state.column <- state.column + 1
        | None -> ()

let rec skip_whitespace state =
    match peek state with
        | Some (' ' | '\t' | '\r' | '\n') ->
            advance state;
            skip_whitespace state
        | Some '/' -> (
            (* Handle // comments *)
                match state.input.[state.pos + 1] with
                | '/' ->
                    while
                      peek state <> Some '\n'
                      && peek state <> None
                    do
                      advance state
                    done;
                    skip_whitespace state
                | _ -> ())
        | _ -> ()

let start_position state : position =
    { line = state.line; column = state.column }

let end_position state : position =
    { line = state.line; column = state.column }

let is_ident_char c =
    match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
        | _ -> false

let lex_ident_or_keyword state start =
    let buffer = Buffer.create 16 in
        while
          Option.map is_ident_char (peek state) = Some true
        do
          Buffer.add_char buffer (Option.get (peek state));
          advance state
        done;
        let text = Buffer.contents buffer in
        let span =
            {
              start_pos = start;
              end_pos = end_position state;
            }
        in
        let node =
            match text with
                | "fn" -> Keyword K_fn
                | "let" -> Keyword K_let
                | "if" -> Keyword K_if
                | "else" -> Keyword K_else
                | "return" -> Keyword K_return
                | "true" -> Bool true
                | "false" -> Bool false
                | _ -> Ident text
        in
            { node; span }

let lex_number state start =
    let buffer = Buffer.create 16 in

    let rec consume_digits () =
        match peek state with
            | Some c
              when match c with
                       | '0' .. '9' -> true
                       | _ -> false ->
                Buffer.add_char buffer c;
                advance state;
                consume_digits ()
            | _ -> ()
    in

    consume_digits ();

    (* Check for decimal point to distinguish float *)
        match peek state with
        | Some '.' ->
            advance state;
            Buffer.add_char buffer '.';
            consume_digits ();
            let text = Buffer.contents buffer in
            let value = float_of_string text in
            let span =
                {
                  start_pos = start;
                  end_pos = end_position state;
                }
            in
                { node = Float value; span }
        | _ ->
            let text = Buffer.contents buffer in
            let value = int_of_string text in
            let span =
                {
                  start_pos = start;
                  end_pos = end_position state;
                }
            in
                { node = Int value; span }

let lex_char state start =
    advance state;

    (* skip opening ' *)
    let ch =
        match peek state with
            | Some '\\' -> (
                advance state;
                match peek state with
                    | Some 'n' ->
                        advance state;
                        '\n'
                    | Some 't' ->
                        advance state;
                        '\t'
                    | Some '\\' ->
                        advance state;
                        '\\'
                    | Some '\'' ->
                        advance state;
                        '\''
                    | Some c ->
                        failwith
                          (Printf.sprintf
                             "Unknown escape sequence: \\%c"
                             c)
                    | None ->
                        failwith
                          "Unterminated escape sequence in \
                           character literal")
            | Some c ->
                advance state;
                c
            | None ->
                failwith "Unterminated character literal"
    in

    (* Expect closing ' *)
    (match peek state with
        | Some '\'' -> advance state
        | _ ->
            failwith
              "Expected closing ' in character literal");

    let span =
        { start_pos = start; end_pos = end_position state }
    in
        { node = Char ch; span }

let next_token state : located_token option =
    skip_whitespace state;
    let start = start_position state in
        match peek state with
            | None -> None
            (* Single character symbols *)
            | Some '(' ->
                advance state;
                Some
                  {
                    node = LParen;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some ')' ->
                advance state;
                Some
                  {
                    node = RParen;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some '{' ->
                advance state;
                Some
                  {
                    node = LBrace;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some '}' ->
                advance state;
                Some
                  {
                    node = RBrace;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some '+' ->
                advance state;
                Some
                  {
                    node = Plus;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some '-' -> (
                advance state;
                match peek state with
                    | Some '>' ->
                        advance state;
                        Some
                          {
                            node = Arrow;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          }
                    | _ ->
                        Some
                          {
                            node = Minus;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          })
            | Some '*' ->
                advance state;
                Some
                  {
                    node = Star;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some '/' ->
                advance state;
                Some
                  {
                    node = Slash;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some '=' -> (
                advance state;
                match peek state with
                    | Some '=' ->
                        advance state;
                        Some
                          {
                            node = EqEq;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          }
                    | _ ->
                        Some
                          {
                            node = Assign;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          })
            | Some '!' -> (
                advance state;
                match peek state with
                    | Some '=' ->
                        advance state;
                        Some
                          {
                            node = NotEq;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          }
                    | _ ->
                        failwith
                          (Printf.sprintf
                             "Unknown character: ! at %d:%d"
                             state.line state.column))
            | Some '<' -> (
                advance state;
                match peek state with
                    | Some '=' ->
                        advance state;
                        Some
                          {
                            node = Le;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          }
                    | _ ->
                        Some
                          {
                            node = Lt;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          })
            | Some '>' -> (
                advance state;
                match peek state with
                    | Some '=' ->
                        advance state;
                        Some
                          {
                            node = Ge;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          }
                    | _ ->
                        Some
                          {
                            node = Gt;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          })
            | Some ',' ->
                advance state;
                Some
                  {
                    node = Comma;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some ':' ->
                advance state;
                Some
                  {
                    node = Colon;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some ';' ->
                advance state;
                Some
                  {
                    node = Semicolon;
                    span =
                      {
                        start_pos = start;
                        end_pos = end_position state;
                      };
                  }
            | Some '&' -> (
                advance state;
                match peek state with
                    | Some '&' ->
                        advance state;
                        Some
                          {
                            node = AndAnd;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          }
                    | _ ->
                        failwith
                          (Printf.sprintf
                             "Unknown character \'&\' at \
                              %d:%d"
                             state.line state.column))
            | Some '|' -> (
                advance state;
                match peek state with
                    | Some '|' ->
                        advance state;
                        Some
                          {
                            node = OrOr;
                            span =
                              {
                                start_pos = start;
                                end_pos = end_position state;
                              };
                          }
                    | _ ->
                        failwith
                          (Printf.sprintf
                             "Unknown character \'|\' at \
                              %d:%d"
                             state.line state.column))
            | Some '\'' -> Some (lex_char state start)
            (* Identifiers and keywords *)
            | Some c
              when Char.code c >= Char.code 'a'
                   && Char.code c <= Char.code 'z' ->
                Some (lex_ident_or_keyword state start)
            (* Numbers *)
            | Some c
              when Char.code c >= Char.code '0'
                   && Char.code c <= Char.code '9' ->
                Some (lex_number state start)
            | Some c ->
                failwith
                  (Printf.sprintf
                     "Unknown character: %c at %d:%d" c
                     state.line state.column)
