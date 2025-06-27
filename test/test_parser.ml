open Plato.Lexer
open Plato.Parser
open Plato.Ast

let input =
    {|
  fn main(x: int, y: bool) -> int {
    let z = 42;
    return z;
  }
|}

let () =
    let lexer = init_lexer input in
    let tokens =
        let rec collect acc =
            match next_token lexer with
                | Some tok -> collect (tok :: acc)
                | None -> List.rev acc
        in
            collect []
    in

    let program = parse_program tokens in
        print_endline (string_of_program program)
