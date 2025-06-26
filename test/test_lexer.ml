let test_input =
    {|
  fn main() -> int {
    let x = 42;
    let y = x + 3.14;
    if x == 45.14 {
        return x;
    } else {
        return y;
    }
  }
|}

let () =
    let state = Plato.Lexer.init_lexer test_input in
    let rec loop () =
        match Plato.Lexer.next_token state with
            | Some { node; span } ->
                Printf.printf "%s @ %s\n"
                  (Plato.Token.show_token node)
                  (Plato.Token.show_span span);
                loop ()
            | None -> ()
    in
        loop ()
