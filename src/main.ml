open Plato

let report_error_with_span msg
    (span_opt : Token.span option) =
    match span_opt with
        | Some { start_pos = { line; column }; _ } ->
            Printf.eprintf "Error at %d:%d - %s\n" line
              column msg
        | None -> Printf.eprintf "Error: %s\n" msg

let () =
    if Array.length Sys.argv < 2 then (
      Printf.eprintf "Usage: plato <file.plato>\n";
      exit 1);

    let filename = Sys.argv.(1) in
    let file = open_in_bin filename in
    let source =
        really_input_string file (in_channel_length file)
    in
    let lexer_state = Lexer.init_lexer source in
    let tokens =
        let rec collect acc =
            match Lexer.next_token lexer_state with
                | Some tok -> collect (tok :: acc)
                | None -> List.rev acc
        in
            collect []
    in

    try
      let ast = Parser.parse_program tokens in
          Typecheck.type_program ast;
          let result = Eval.run_program ast in
              Printf.printf "Program result: %s\n"
                (Eval.string_of_value result)
    with
        | Lexer.LexerError (msg, span) ->
            report_error_with_span
              ("Lexer error: " ^ msg)
              span;
            exit 1
        | Parser.ParserError (msg, span) ->
            report_error_with_span
              ("Parse error: " ^ msg)
              span;
            exit 1
        | Typecheck.TypeError (msg, span) ->
            report_error_with_span
              ("Type error: " ^ msg)
              span;
            exit 1
