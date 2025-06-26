type lexer_state = {
  input : string;
  length : int;
  mutable pos : int;
  mutable line : int;
  mutable column : int;
}

let init_lexer input =
  { input; length = String.length input; pos = 0; line = 1; column = 1 }

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
