let test_input = {|
  fn main() -> int {
    let x = 42;
    let y = x + 3.14;
    return y;
  }
|}

let () =
  let state = Plato.Lexer.init_lexer test_input in
  let rec loop () =
    match Plato.Lexer.next_token state with
    | Some { node; span = _ } ->
        print_endline (Plato.Token.show_token node);
        loop ()
    | None -> ()
  in
  loop ()
