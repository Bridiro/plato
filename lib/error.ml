(* Position information *)
type position = {
  line : int;
  column : int;
  offset : int;
}

type span = {
  start_pos : position;
  end_pos : position;
  filename : string option;
}

(* Error types *)
type error_kind =
  | LexError of string
  | ParseError of string
  | SemanticError of string
  | TypeError of string
  | UnknownError of string

type error = {
  kind : error_kind;
  span : span;
  message : string;
}

(* Global source lines for context display *)
let source_lines = ref [||]
let set_source_lines lines = source_lines := lines

(* Error creation functions *)
let make_position line column offset = { line; column; offset }
let make_span start_pos end_pos filename = { start_pos; end_pos; filename }
let make_error kind span message = { kind; span; message }

(* Error formatting *)
let string_of_error_kind = function
  | LexError _ -> "Lexer Error"
  | ParseError _ -> "Parser Error"
  | SemanticError _ -> "Semantic Error"
  | TypeError _ -> "Type Error"
  | UnknownError _ -> "Unknown Error"

let format_position pos =
  Printf.sprintf "line %d, column %d" pos.line pos.column

let format_span span =
  match span.filename with
  | Some filename ->
    if span.start_pos.line = span.end_pos.line then
      Printf.sprintf "%s:%d:%d" filename span.start_pos.line
        span.start_pos.column
    else
      Printf.sprintf "%s:%d:%d-%d:%d" filename span.start_pos.line
        span.start_pos.column span.end_pos.line span.end_pos.column
  | None ->
    if span.start_pos.line = span.end_pos.line then
      Printf.sprintf "%d:%d" span.start_pos.line span.start_pos.column
    else
      Printf.sprintf "%d:%d-%d:%d" span.start_pos.line span.start_pos.column
        span.end_pos.line span.end_pos.column

(* Enhanced error context display *)
let show_error_with_context error =
  let line_num = error.span.start_pos.line in
  let col_num = error.span.start_pos.column in
  let end_col = error.span.end_pos.column in

  let error_header =
    Printf.sprintf "%s: %s at %s"
      (string_of_error_kind error.kind)
      error.message (format_span error.span)
  in

  if line_num > 0 && line_num <= Array.length !source_lines then
    let line = !source_lines.(line_num - 1) in
    let line_len = String.length line in
    let start_col = max 0 (col_num - 1) in
    let end_col_adj =
      min line_len
        (if end_col > col_num then
           end_col - 1
         else
           start_col)
    in

    (* Create pointer with underline for ranges *)
    let pointer =
      if end_col_adj > start_col then
        String.make start_col ' '
        ^ String.make (end_col_adj - start_col + 1) '^'
      else
        String.make start_col ' ' ^ "^"
    in

    Printf.sprintf "%s\n\n%4d | %s\n     | %s" error_header line_num line
      pointer
  else
    error_header

(* Legacy function for backward compatibility *)
let show_error_context error source_lines_array =
  let old_source = !source_lines in
  source_lines := source_lines_array ;
  let result = show_error_with_context error in
  source_lines := old_source ;
  result

let format_error error = show_error_with_context error

(* Exception type for carrying position info *)
exception CompilerError of error

(* Convenience functions for creating errors *)
let lex_error ~filename ~line ~column ~offset message =
  let pos = make_position line column offset in
  let span = make_span pos pos filename in
  CompilerError (make_error (LexError message) span message)

let parse_error ~filename ~line ~column ~offset message =
  let pos = make_position line column offset in
  let span = make_span pos pos filename in
  CompilerError (make_error (ParseError message) span message)

let semantic_error ~filename ~line ~column ~offset message =
  let pos = make_position line column offset in
  let span = make_span pos pos filename in
  CompilerError (make_error (SemanticError message) span message)

let type_error ~filename ~line ~column ~offset message =
  let pos = make_position line column offset in
  let span = make_span pos pos filename in
  CompilerError (make_error (TypeError message) span message)

(* Enhanced error creation with spans *)
let lex_error_span
    ~filename
    ~start_line
    ~start_col
    ~end_line
    ~end_col
    ~offset
    message =
  let start_pos = make_position start_line start_col offset in
  let end_pos =
    make_position end_line end_col (offset + (end_col - start_col))
  in
  let span = make_span start_pos end_pos filename in
  CompilerError (make_error (LexError message) span message)

let parse_error_span
    ~filename
    ~start_line
    ~start_col
    ~end_line
    ~end_col
    ~offset
    message =
  let start_pos = make_position start_line start_col offset in
  let end_pos =
    make_position end_line end_col (offset + (end_col - start_col))
  in
  let span = make_span start_pos end_pos filename in
  CompilerError (make_error (ParseError message) span message)

let type_error_span
    ~filename
    ~start_line
    ~start_col
    ~end_line
    ~end_col
    ~offset
    message =
  let start_pos = make_position start_line start_col offset in
  let end_pos =
    make_position end_line end_col (offset + (end_col - start_col))
  in
  let span = make_span start_pos end_pos filename in
  CompilerError (make_error (TypeError message) span message)
