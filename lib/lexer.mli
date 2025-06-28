open Token

type lexer_state

exception LexerError of string * span option

val init_lexer : string -> lexer_state
val next_token : lexer_state -> Token.located_token option
