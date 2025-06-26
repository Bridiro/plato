type lexer_state

val init_lexer : string -> lexer_state
val next_token : lexer_state -> Token.located_token option
