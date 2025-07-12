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

(* Error creation functions *)
let make_position line column offset =
  { line; column; offset }

let make_span start_pos end_pos filename =
  { start_pos; end_pos; filename }

let make_error kind span message =
  { kind; span; message }

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
        Printf.sprintf "%s:%d:%d" filename span.start_pos.line span.start_pos.column
      else
        Printf.sprintf "%s:%d:%d-%d:%d" filename 
          span.start_pos.line span.start_pos.column
          span.end_pos.line span.end_pos.column
  | None ->
      if span.start_pos.line = span.end_pos.line then
        Printf.sprintf "%d:%d" span.start_pos.line span.start_pos.column
      else
        Printf.sprintf "%d:%d-%d:%d" 
          span.start_pos.line span.start_pos.column
          span.end_pos.line span.end_pos.column

let format_error error =
  Printf.sprintf "%s: %s at %s\n%s"
    (string_of_error_kind error.kind)
    error.message
    (format_span error.span)
    ""

(* Helper to show source context *)
let show_error_context error source_lines =
  let line_num = error.span.start_pos.line in
  let col_num = error.span.start_pos.column in
  
  if line_num > 0 && line_num <= Array.length source_lines then
    let line = source_lines.(line_num - 1) in
    let pointer = String.make (max 0 (col_num - 1)) ' ' ^ "^" in
    Printf.sprintf "%s\n%4d | %s\n     | %s"
      (format_error error)
      line_num
      line
      pointer
  else
    format_error error

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
