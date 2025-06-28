open Token

exception ParserError of string * span option

val parse_program : Token.located_token list -> Ast.program
