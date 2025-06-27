open Plato.Lexer
open Plato.Parser
open Plato.Ast

let input =
    {|
    fn add(x: int, y: int) -> int {
        return x + y * 1;
    }

    fn main() -> int {
        let a = 3;
        let b = 10;
        let c = true && false || true;
        let x = add(a, b);
        return 0;
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
