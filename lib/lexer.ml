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
        | Some '/'
          when state.pos + 1 < state.length
               && state.input.[state.pos + 1] = '/' ->
            advance state;
            (* Consume first '/' *)
            advance state;
            (* Consume second '/' *)
            while
              peek state <> Some '\n' && peek state <> None
            do
              advance state
            done;
            skip_whitespace state
            (* Continue skipping any whitespace/comments after this line *)
        | _ -> ()

let start_position state : position =
    { line = state.line; column = state.column }

let end_position state : position =
    { line = state.line; column = state.column }

(* Helper to create a located token *)
let make_located_token node start_pos state =
    Some
      {
        node;
        span = { start_pos; end_pos = end_position state };
      }

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
            make_located_token node start state

let lex_number state start =
    let buffer = Buffer.create 16 in

    let rec consume_digits () =
        match peek state with
            | Some c when c >= '0' && c <= '9' ->
                Buffer.add_char buffer c;
                advance state;
                consume_digits ()
            | _ -> ()
    in

    consume_digits ();

    match peek state with
        | Some '.' ->
            advance state;
            Buffer.add_char buffer '.';
            consume_digits ();
            let text = Buffer.contents buffer in
            let value = float_of_string text in
                make_located_token (Float value) start state
        | _ ->
            let text = Buffer.contents buffer in
            let value = int_of_string text in
                make_located_token (Int value) start state

let lex_char state start =
    advance state;

    (* Skip opening '\'' *)
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

    (* Expect closing '\'' *)
    (match peek state with
        | Some '\'' -> advance state
        | _ ->
            failwith
              "Expected closing ' in character literal");

    make_located_token (Char ch) start state

let next_token state : located_token option =
    skip_whitespace state;
    let start = start_position state in

    (* Helper for single-character tokens *)
    let advance_and_make_token node =
        advance state;
        make_located_token node start state
    in

    match peek state with
        | None -> None
        | Some c -> (
            match c with
                (* Single character symbols *)
                | '(' -> advance_and_make_token LParen
                | ')' -> advance_and_make_token RParen
                | '{' -> advance_and_make_token LBrace
                | '}' -> advance_and_make_token RBrace
                | '+' -> advance_and_make_token Plus
                | '*' -> advance_and_make_token Star
                | '/' -> advance_and_make_token Slash
                | ',' -> advance_and_make_token Comma
                | ':' -> advance_and_make_token Colon
                | ';' -> advance_and_make_token Semicolon
                (* Multi-character or context-dependent tokens *)
                | '-' -> (
                    advance state;
                    match peek state with
                        | Some '>' ->
                            advance_and_make_token Arrow
                        | _ ->
                            make_located_token Minus start
                              state)
                | '=' -> (
                    advance state;
                    match peek state with
                        | Some '=' ->
                            advance_and_make_token EqEq
                        | _ ->
                            make_located_token Assign start
                              state)
                | '!' -> (
                    advance state;
                    match peek state with
                        | Some '=' ->
                            advance_and_make_token NotEq
                        | _ ->
                            failwith
                              (Printf.sprintf
                                 "Unknown character: ! at \
                                  %d:%d"
                                 state.line state.column))
                | '<' -> (
                    advance state;
                    match peek state with
                        | Some '=' ->
                            advance_and_make_token Le
                        | _ ->
                            make_located_token Lt start
                              state)
                | '>' -> (
                    advance state;
                    match peek state with
                        | Some '=' ->
                            advance_and_make_token Ge
                        | _ ->
                            make_located_token Gt start
                              state)
                | '&' -> (
                    advance state;
                    match peek state with
                        | Some '&' ->
                            advance_and_make_token AndAnd
                        | _ ->
                            failwith
                              (Printf.sprintf
                                 "Unknown character '&' at \
                                  %d:%d"
                                 state.line state.column))
                | '|' -> (
                    advance state;
                    match peek state with
                        | Some '|' ->
                            advance_and_make_token OrOr
                        | _ ->
                            failwith
                              (Printf.sprintf
                                 "Unknown character '|' at \
                                  %d:%d"
                                 state.line state.column))
                (* Character type *)
                | '\'' -> lex_char state start
                (* Literals and Keywords *)
                | 'a' .. 'z' | 'A' .. 'Z' ->
                    lex_ident_or_keyword state start
                (* Numbers *)
                | '0' .. '9' -> lex_number state start
                (* Default case -- error *)
                | _ ->
                    failwith
                      (Printf.sprintf
                         "Unknown character: %c at %d:%d" c
                         state.line state.column))
