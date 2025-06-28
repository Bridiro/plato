open Plato.Parser
open Plato.Eval
open Plato.Lexer

let () =
    let source =
        {|
    let x = 2;

    fn add(a: int, b: int) -> int {
      return a + b;
    }

    fn main() -> int {
      let y = add(x, 7);
      if y > 10 {
        return 42;
      } else {
        return 0;
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
