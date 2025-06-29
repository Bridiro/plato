open Plato.Parser
open Plato.Eval
open Plato.Lexer

let () =
    let source =
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
    in

    let lexer = init_lexer source in
    let tokens =
        let rec collect acc =
            match next_token lexer with
                | Some tok -> collect (tok :: acc)
                | None -> List.rev acc
        in
            collect []
    in

    let program = parse_program tokens in

    let result = run_program program in
        print_endline
          ("Program returned: "
          ^ string_of_value result)
