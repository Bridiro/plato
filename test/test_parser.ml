let test_simple_function = {|
fn main() -> i32 {
  let x = 42;
  x
}
|}

let test_complex_function = {|
fn factorial(n: i32) -> i32 {
  if n <= 1 {
    1
  } else {
    n * factorial(n - 1)
  }
}
|}

let test_struct = {|
struct Point {
  x: f64,
  y: f64
}
|}

let test_enum = {|
enum Color {
  Red,
  Green,
  Blue
}
|}

let parse_test name input =
  Printf.printf "Testing %s:\n" name;
  try
    let (lexer_fn, lexbuf) = Plato.Lexer.parse_string input in
    let ast = Plato.Parser.program lexer_fn lexbuf in
    Printf.printf "  ✓ Parsed successfully\n";
    Printf.printf "  AST: %d top-level items\n" (List.length ast);
    flush_all ()
  with
  | Plato.Lexer.LexError msg ->
      Printf.printf "  ✗ Lexer error: %s\n" msg;
      flush_all ()
  | Plato.Parser.Error ->
      Printf.printf "  ✗ Parser error\n";
      flush_all ()
  | e ->
      Printf.printf "  ✗ Other error: %s\n" (Printexc.to_string e);
      flush_all ()

let () =
  Printf.printf "=== Plato Parser Tests ===\n\n";
  parse_test "Simple function" test_simple_function;
  parse_test "Complex function" test_complex_function;
  parse_test "Struct definition" test_struct;
  parse_test "Enum definition" test_enum;
  Printf.printf "\nDone.\n"
