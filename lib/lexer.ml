open Ast
open Parser

exception LexError of string

(* Use the parser's token type *)
let keywords =
  [
    ("fn", FN);
    ("let", LET);
    ("mut", MUT);
    ("if", IF);
    ("else", ELSE);
    ("while", WHILE);
    ("for", FOR);
    ("loop", LOOP);
    ("break", BREAK);
    ("continue", CONTINUE);
    ("return", RETURN);
    ("match", MATCH);
    ("struct", STRUCT);
    ("enum", ENUM);
    ("impl", IMPL);
    ("trait", TRAIT);
    ("use", USE);
    ("mod", MOD);
    ("pub", PUB);
    ("const", CONST);
    ("static", STATIC);
    ("true", TRUE);
    ("false", FALSE);
    ("as", AS);
    ("type", TYPE);
    ("in", IN);
    ("sizeof", SIZEOF);
    ("null", NULL);
    ("usize", USIZE);
  ]

let keyword_table = Hashtbl.create 32
let () = List.iter (fun (k, v) -> Hashtbl.add keyword_table k v) keywords
let is_keyword s = Hashtbl.mem keyword_table s
let get_keyword s = Hashtbl.find keyword_table s

(* Use the parser's token type *)
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'

let is_hex_digit c =
  is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let is_octal_digit c = c >= '0' && c <= '7'
let is_binary_digit c = c = '0' || c = '1'
let is_alnum c = is_alpha c || is_digit c
let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let parse_integer_suffix s : integer_suffix option =
  match s with
  | "i8" -> Some I8
  | "i16" -> Some I16
  | "i32" -> Some I32
  | "i64" -> Some I64
  | "u8" -> Some U8
  | "u16" -> Some U16
  | "u32" -> Some U32
  | "u64" -> Some U64
  | "usize" -> Some Usize
  | _ -> None

let parse_float_suffix s : float_suffix option =
  match s with
  | "f32" -> Some F32
  | "f64" -> Some F64
  | _ -> None

let parse_escape_char c =
  match c with
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | '\\' -> '\\'
  | '"' -> '"'
  | '\'' -> '\''
  | '0' -> '\000'
  | _ -> raise (LexError ("Invalid escape sequence: \\" ^ String.make 1 c))

type lexer_state = {
  input : string;
  pos : int;
  line : int;
  column : int;
  length : int;
}

let create_lexer input =
  { input; pos = 0; line = 1; column = 1; length = String.length input }

let current_char state =
  if state.pos >= state.length then
    None
  else
    Some state.input.[state.pos]

let peek_char state offset =
  let pos = state.pos + offset in
  if pos >= state.length then
    None
  else
    Some state.input.[pos]

let advance state =
  if state.pos >= state.length then
    state
  else
    let c = state.input.[state.pos] in
    if c = '\n' then
      { state with pos = state.pos + 1; line = state.line + 1; column = 1 }
    else
      { state with pos = state.pos + 1; column = state.column + 1 }

let advance_n state n =
  let rec loop s i =
    if i = 0 then
      s
    else
      loop (advance s) (i - 1)
  in
  loop state n

let rec skip_whitespace state =
  match current_char state with
  | Some c when is_whitespace c -> skip_whitespace (advance state)
  | _ -> state

let rec skip_line_comment state =
  match current_char state with
  | Some '\n' -> advance state
  | Some _ -> skip_line_comment (advance state)
  | None -> state

let rec skip_block_comment state depth =
  match (current_char state, peek_char state 1) with
  | Some '/', Some '*' -> skip_block_comment (advance_n state 2) (depth + 1)
  | Some '*', Some '/' ->
    let new_state = advance_n state 2 in
    if depth = 1 then
      new_state
    else
      skip_block_comment new_state (depth - 1)
  | Some _, _ -> skip_block_comment (advance state) depth
  | None, _ -> raise (LexError "Unterminated block comment")

let rec skip_comments_and_whitespace state =
  let state = skip_whitespace state in
  match (current_char state, peek_char state 1) with
  | Some '/', Some '/' ->
    skip_comments_and_whitespace (skip_line_comment (advance_n state 2))
  | Some '/', Some '*' ->
    skip_comments_and_whitespace (skip_block_comment (advance_n state 2) 1)
  | _ -> state

let read_identifier state =
  let start_pos = state.pos in
  let rec loop s =
    match current_char s with
    | Some c when is_alnum c || c = '_' -> loop (advance s)
    | _ -> s
  in
  let end_state = loop state in
  let len = end_state.pos - start_pos in
  let id = String.sub state.input start_pos len in
  (id, end_state)

let read_number state =
  let start_pos = state.pos in

  let read_digits s pred =
    let rec loop s =
      match current_char s with
      | Some c when pred c -> loop (advance s)
      | _ -> s
    in
    loop s
  in

  let base, start_state =
    match (current_char state, peek_char state 1) with
    | Some '0', Some 'x' -> (16, advance_n state 2)
    | Some '0', Some 'o' -> (8, advance_n state 2)
    | Some '0', Some 'b' -> (2, advance_n state 2)
    | _ -> (10, state)
  in

  let pred =
    match base with
    | 2 -> is_binary_digit
    | 8 -> is_octal_digit
    | 10 -> is_digit
    | 16 -> is_hex_digit
    | _ -> failwith "Invalid base"
  in

  let after_int = read_digits start_state pred in

  (* Check for float *)
  let is_float, after_float =
    if base = 10 then
      match current_char after_int with
      | Some '.' ->
        let after_dot = advance after_int in
        (true, read_digits after_dot is_digit)
      | _ -> (false, after_int)
    else
      (false, after_int)
  in

  (* Read suffix *)
  let suffix, end_state =
    let rec loop s acc =
      match current_char s with
      | Some c when is_alpha c -> loop (advance s) (acc ^ String.make 1 c)
      | _ -> (acc, s)
    in
    loop after_float ""
  in

  let num_part =
    String.sub state.input start_pos (after_float.pos - start_pos)
  in

  if is_float then
    let float_val = float_of_string num_part in
    let float_suffix =
      if suffix = "" then
        None
      else
        parse_float_suffix suffix
    in
    (FLOAT (float_val, float_suffix), end_state)
  else
    (* Fix the integer parsing for different bases *)
    let int_val =
      if base = 10 then
        int_of_string num_part
      else
        int_of_string
          num_part (* OCaml's int_of_string handles 0x, 0o, 0b prefixes *)
    in
    let int_suffix =
      if suffix = "" then
        None
      else
        parse_integer_suffix suffix
    in
    (INTEGER (int_val, int_suffix), end_state)

let read_string state =
  let start_state = advance state in
  (* Skip opening quote *)
  let buffer = Buffer.create 16 in

  let rec loop s =
    match current_char s with
    | Some '"' -> advance s
    | Some '\\' -> begin
      match peek_char s 1 with
      | Some c ->
        let escaped = parse_escape_char c in
        Buffer.add_char buffer escaped ;
        loop (advance_n s 2)
      | None -> raise (LexError "Unterminated string literal")
    end
    | Some c ->
      Buffer.add_char buffer c ;
      loop (advance s)
    | None -> raise (LexError "Unterminated string literal")
  in

  let end_state = loop start_state in
  let str_val = Buffer.contents buffer in
  (STRING str_val, end_state)

let read_char state =
  let start_state = advance state in
  (* Skip opening quote *)

  let char_val, after_char =
    match current_char start_state with
    | Some '\\' -> begin
      match peek_char start_state 1 with
      | Some c -> (parse_escape_char c, advance_n start_state 2)
      | None -> raise (LexError "Unterminated character literal")
    end
    | Some c -> (c, advance start_state)
    | None -> raise (LexError "Unterminated character literal")
  in

  let end_state =
    match current_char after_char with
    | Some '\'' -> advance after_char
    | _ -> raise (LexError "Unterminated character literal")
  in

  (CHAR char_val, end_state)

let next_token state =
  let state = skip_comments_and_whitespace state in

  match current_char state with
  | None -> (EOF, state)
  | Some c ->
    (match c with
    | '+' -> begin
      match peek_char state 1 with
      | Some '=' -> (PLUS_ASSIGN, advance_n state 2)
      | _ -> (PLUS, advance state)
    end
    | '-' -> begin
      match peek_char state 1 with
      | Some '=' -> (MINUS_ASSIGN, advance_n state 2)
      | Some '>' -> (ARROW, advance_n state 2)
      | _ -> (MINUS, advance state)
    end
    | '*' -> begin
      match peek_char state 1 with
      | Some '=' -> (STAR_ASSIGN, advance_n state 2)
      | _ -> (STAR, advance state)
    end
    | '/' -> begin
      match peek_char state 1 with
      | Some '=' -> (SLASH_ASSIGN, advance_n state 2)
      | _ -> (SLASH, advance state)
    end
    | '%' -> begin
      match peek_char state 1 with
      | Some '=' -> (PERCENT_ASSIGN, advance_n state 2)
      | _ -> (PERCENT, advance state)
    end
    | '=' -> begin
      match peek_char state 1 with
      | Some '=' -> (EQ, advance_n state 2)
      | _ -> (ASSIGN, advance state)
    end
    | '!' -> begin
      match peek_char state 1 with
      | Some '=' -> (NE, advance_n state 2)
      | _ -> (NOT, advance state)
    end
    | '<' -> begin
      match peek_char state 1 with
      | Some '=' -> (LE, advance_n state 2)
      | Some '<' -> begin
        match peek_char state 2 with
        | Some '=' -> (SHL_ASSIGN, advance_n state 3)
        | _ -> (SHL, advance_n state 2)
      end
      | _ -> (LT, advance state)
    end
    | '>' -> begin
      match peek_char state 1 with
      | Some '=' -> (GE, advance_n state 2)
      | Some '>' -> begin
        match peek_char state 2 with
        | Some '=' -> (SHR_ASSIGN, advance_n state 3)
        | _ -> (SHR, advance_n state 2)
      end
      | _ -> (GT, advance state)
    end
    | '&' -> begin
      match peek_char state 1 with
      | Some '&' -> (AND, advance_n state 2)
      | Some '=' -> (BIT_AND_ASSIGN, advance_n state 2)
      | _ -> (BIT_AND, advance state)
    end
    | '|' -> begin
      match peek_char state 1 with
      | Some '|' -> (OR, advance_n state 2)
      | Some '=' -> (BIT_OR_ASSIGN, advance_n state 2)
      | _ -> (BIT_OR, advance state)
    end
    | '^' -> begin
      match peek_char state 1 with
      | Some '=' -> (BIT_XOR_ASSIGN, advance_n state 2)
      | _ -> (BIT_XOR, advance state)
    end
    | ':' -> begin
      match peek_char state 1 with
      | Some ':' -> (DOUBLE_COLON, advance_n state 2)
      | _ -> (COLON, advance state)
    end
    | '.' -> begin
      match peek_char state 1 with
      | Some '.' -> (DOTDOT, advance_n state 2)
      | _ -> (DOT, advance state)
    end
    | ',' -> (COMMA, advance state)
    | ';' -> (SEMICOLON, advance state)
    | '?' -> (QUESTION, advance state)
    | '(' -> (LPAREN, advance state)
    | ')' -> (RPAREN, advance state)
    | '[' -> (LBRACKET, advance state)
    | ']' -> (RBRACKET, advance state)
    | '{' -> (LBRACE, advance state)
    | '}' -> (RBRACE, advance state)
    | '"' -> read_string state
    | '\'' -> read_char state
    | c when is_digit c -> read_number state
    | c when is_alpha c || c = '_' ->
      let id, new_state = read_identifier state in
      let token =
        if is_keyword id then
          get_keyword id
        else
          IDENTIFIER id
      in
      (token, new_state)
    | _ -> raise (LexError ("Unexpected character: " ^ String.make 1 c)))

let tokenize input =
  let state = create_lexer input in
  let tokens = ref [] in
  let rec loop s =
    let token, new_state = next_token s in
    tokens := token :: !tokens ;
    if token = EOF then
      List.rev !tokens
    else
      loop new_state
  in
  loop state

(* Menhir bridge - convert our lexer to work with Menhir's expectations *)
let create_menhir_lexer input =
  let state = ref (create_lexer input) in
  fun _lexbuf ->
    let (token, new_state) = next_token !state in
    state := new_state;
    token

(* Helper function to parse a string directly *)
let parse_string input =
  let lexbuf = Lexing.from_string input in
  let lexer_fn = create_menhir_lexer input in
  (lexer_fn, lexbuf)
