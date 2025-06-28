open Plato.Parser
open Plato.Eval
open Plato.Lexer

let () =
    let source =
        {|
    let a = 20;

    fn add(x: int, y: int) -> int {
        return x + y * 1;
    }

    fn main() -> int {
        let a = 3;
        let b = 10;
        let c = true && false || true;
        let x = add(a, b);
        if x >= 10 && c {
            2
        } else {
            1
        }
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
