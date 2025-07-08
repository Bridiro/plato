let test_input =
  {|
  fn main() -> int {
    let x = 42;
    let y = x + 3.14;
    if x == 45.14 {
        x
    } else {
        y
    }
  }
|}
in
try
  let _ = Plato.Lexer.tokenize test_input in
  flush_all ()
with
| e -> 
  Printf.printf "Exception: %s\n" (Printexc.to_string e);
  flush_all ()
