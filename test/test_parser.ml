open Plato.Lexer
open Plato.Parser
open Plato.Ast

let input =
    {|
let a: int = 3;
let b: bool = true;

fn mul(x: int, y: int) -> int {
    let r = x * y;
    r
}

fn main() -> int {
    if b {
        return mul(a, 2);
    }
    a
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
