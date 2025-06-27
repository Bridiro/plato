open Plato.Ast

let input =
    {|
  let x = 42;
  let y = true;
  let c = 'z';
  let z = c;
  return x;
|}

let () =
    let lexer = Plato.Lexer.init_lexer input in
    let tokens =
        let rec collect acc =
            match Plato.Lexer.next_token lexer with
                | Some tok -> collect (tok :: acc)
                | None -> List.rev acc
        in
            collect []
    in

    let ast = Plato.Parser.parse_program tokens in

    List.iter
      (fun stmt ->
        match stmt with
            | Let (name, expr) ->
                Printf.printf "Let %s = %s\n" name
                  (match expr with
                      | Int i -> string_of_int i
                      | Bool b -> string_of_bool b
                      | Char c -> Printf.sprintf "'%c'" c
                      | Ident id -> id)
            | Return e ->
                Printf.printf "Return %s\n"
                  (match e with
                      | Int i -> string_of_int i
                      | Bool b -> string_of_bool b
                      | Char c -> Printf.sprintf "'%c'" c
                      | Ident id -> id)
            | Expr _ -> Printf.printf "Expr\n")
      ast
