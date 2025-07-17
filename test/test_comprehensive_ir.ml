(* Comprehensive test for every EBNF feature *)

(* Helper function to test parsing *)
let test_parse name code =
  Printf.printf "\n=== Testing %s ===\n" name;
  Printf.printf "Code: %s\n" code;
  try
    let lexer_fn, lexbuf = Eiron.Lexer.parse_string code in
    let ast = Eiron.Parser.program lexer_fn lexbuf in
    Printf.printf "✓ Parse: SUCCESS\n";
    
    (* Test type checking *)
    (try
      Eiron.Type_checker.type_check_program ast "test_code";
      Printf.printf "✓ Type Check: SUCCESS\n";
      
      (* Test IR generation *)
      (try
        let ir_module = Eiron.Ast_to_ir.convert_ast_to_ir ast in
        Printf.printf "✓ IR Generation: SUCCESS\n";
        Printf.printf "Generated IR:\n%s\n" (Eiron.Ir.string_of_ir_module ir_module);
        true
      with exn ->
        Printf.printf "✗ IR Generation: %s\n" (Printexc.to_string exn);
        false)
    with exn ->
      Printf.printf "✗ Type Check: %s\n" (Printexc.to_string exn);
      false)
  with
  | Eiron.Lexer.LexError (err, line, column) -> 
    Printf.printf "✗ Lexer error at %d:%d: %s\n" line column err; false
  | Eiron.Parser.Error -> 
    Printf.printf "✗ Parser error\n"; false
  | exn -> 
    Printf.printf "✗ Exception: %s\n" (Printexc.to_string exn); false

(* Test categories based on EBNF *)

let test_literals () =
  Printf.printf "\n\n🔢 TESTING LITERALS\n";
  Printf.printf "===================\n";
  let tests = [
    ("Integer literals", "fn test() -> i32 { 42 }");
    ("Hex literals", "fn test() -> i32 { 0xFF }");
    ("Binary literals", "fn test() -> i32 { 0b1010 }");
    ("Octal literals", "fn test() -> i32 { 0o777 }");
    ("Float literals", "fn test() -> f64 { 3.14 }");
    ("String literals", "fn test() -> str { \"hello world\" }");
    ("Char literals", "fn test() -> char { 'x' }");
    ("Bool literals", "fn test() -> bool { true }");
    ("Unit literal", "fn test() { () }");
    ("Null literal", "fn test() -> *i32 { null }");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_types () =
  Printf.printf "\n\n📝 TESTING TYPES\n";
  Printf.printf "================\n";
  let tests = [
    ("Primitive types", "fn test(x: i32, y: f64, z: bool) -> void { }");
    ("Array types", "fn test() -> [i32; 5] { [1, 2, 3, 4, 5] }");
    ("Pointer types", "fn test() -> *i32 { null }");
    ("Function types", "type FnPtr = fn(i32, i32) -> i32;");
    ("Path types", "struct Point { x: i32 } fn test() -> Point { Point { x: 1 } }");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_expressions () =
  Printf.printf "\n\n🔣 TESTING EXPRESSIONS\n";
  Printf.printf "======================\n";
  let tests = [
    ("Arithmetic", "fn test() -> i32 { 1 + 2 * 3 - 4 / 2 % 3 }");
    ("Comparison", "fn test() -> bool { 1 < 2 && 3 > 1 || 4 == 4 }");
    ("Logical", "fn test() -> bool { true && false || !true }");
    ("Bitwise", "fn test() -> i32 { 1 & 2 | 3 ^ 4 << 1 >> 2 }");
    ("Unary", "fn test() -> i32 { -x + &y * sizeof(i32) }");
    ("Cast", "fn test() -> f64 { 42 as f64 }");
    ("Pointer access", "fn test() -> i32 { ptr->field }");
    ("Function call", "fn test() -> i32 { func(1, 2) }");
    ("Range", "fn test() { for i in 1..10 { } }");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_statements () =
  Printf.printf "\n\n📋 TESTING STATEMENTS\n";
  Printf.printf "=====================\n";
  let tests = [
    ("Let statement", "fn test() { let x = 42; }");
    ("Mutable let", "fn test() { let mut x = 42; }");
    ("Typed let", "fn test() { let x: i32 = 42; }");
    ("Assignment", "fn test() { let x: i32; x = 42; }");
    ("Compound assignment", "fn test() { let x = 0; x += 42; x *= 2; x >>= 1; }");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_control_flow () =
  Printf.printf "\n\n🔀 TESTING CONTROL FLOW\n";
  Printf.printf "========================\n";
  let tests = [
    ("If expression", "fn test() -> i32 { if true { 1 } else { 2 } }");
    ("If statement", "fn test(x: i32) { if x > 0 { return; } }");
    ("While loop", "fn test() { while true { break; } }");
    ("For loop", "fn test() { for i in 1..10 { continue; } }");
    ("Infinite loop", "fn test() { loop { break 42; } }");
    ("Return", "fn test() -> i32 { return 42; }");
    ("Break with value", "fn test() -> i32 { loop { break 42; } }");
    ("Continue", "fn test() { loop { continue; } }");
    ("Nested control", "fn test() { for i in 1..5 { if i == 3 { break; } } }");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_data_structures () =
  Printf.printf "\n\n🏗️  TESTING DATA STRUCTURES\n";
  Printf.printf "============================\n";
  let tests = [
    ("Struct definition", "struct Point { x: i32, y: i32 }");
    ("Struct with visibility", "pub struct Point { pub x: i32, y: i32 }");
    ("Struct initialization", "fn test() -> Point { Point { x: 1, y: 2 } }");
    ("Enum definition", "enum Color { Red, Green, Blue }");
    ("Enum with data", "enum Option { None, Some(i32) }");
    ("Enum with values", "enum Status { Ok = 0, Error = 1 }");
    ("Array literal", "fn test() -> [i32; 3] { [1, 2, 3] }");
    ("Empty array", "fn test() -> [i32; 0] { [] }");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_functions () =
  Printf.printf "\n\n🔧 TESTING FUNCTIONS\n";
  Printf.printf "====================\n";
  let tests = [
    ("Simple function", "fn add(x: i32, y: i32) -> i32 { x + y }");
    ("No params", "fn test() -> i32 { 42 }");
    ("No return", "fn test() { }");
    ("Public function", "pub fn test() -> i32 { 42 }");
    ("Method definition", "impl Point { fn new(x: i32, y: i32) -> Point { Point { x, y } } }");
    ("Recursive function", "fn factorial(n: i32) -> i32 { if n <= 1 { 1 } else { n * factorial(n - 1) } }");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_advanced_features () =
  Printf.printf "\n\n🚀 TESTING ADVANCED FEATURES\n";
  Printf.printf "=============================\n";
  let tests = [
    ("Match expression", "fn test(x: i32) -> i32 { match x { 1 => 10, 2 => 20, _ => 0 } }");
    ("Complex patterns", "fn test(opt: Option) -> i32 { match opt { None => 0, Some(x) => x } }");
    ("Pointer operations", "fn test() { let x = 42; let p = &x; let y = *p; }");
    ("Sizeof operator", "fn test() -> usize { sizeof(i32) }");
    ("Type alias", "type Int = i32; fn test() -> Int { 42 }");
    ("Global variables", "const PI: f64 = 3.14159; fn test() -> f64 { PI }");
    ("Static variables", "static mut counter: i32 = 0;");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_edge_cases () =
  Printf.printf "\n\n⚠️  TESTING EDGE CASES\n";
  Printf.printf "======================\n";
  let tests = [
    ("Empty program", "");
    ("Only comments", "// This is a comment\n/* Block comment */");
    ("Nested blocks", "fn test() { { { let x = 1; } } }");
    ("Complex expressions", "fn test() -> i32 { (1 + 2) * (3 - 4) / (5 % 6) }");
    ("Multiple statements", "fn test() { let x = 1; let y = 2; x + y; }");
    ("Trailing commas", "fn test(x: i32, y: i32,) -> i32 { func(1, 2,) }");
    ("Very long identifiers", "fn very_long_function_name_that_tests_identifier_limits() { }");
  ] in
  List.fold_left (fun acc (name, code) -> 
    (test_parse name code) && acc) true tests

let test_complete_programs () =
  Printf.printf "\n\n📋 TESTING COMPLETE PROGRAMS\n";
  Printf.printf "=============================\n";
  
  let program1 = "
// Complete program with all features
const MAX_SIZE: usize = 100;
static mut global_counter: i32 = 0;

enum Status {
    Ok = 0,
    Error = 1,
    Pending = 2
}

struct Point {
    x: i32,
    y: i32
}

struct Rectangle {
    top_left: Point,
    bottom_right: Point
}

impl Point {
    fn new(x: i32, y: i32) -> Point {
        Point { x: x, y: y }
    }
    
    fn distance_from_origin(self: *Point) -> f64 {
        let x_sq = (self.x * self.x) as f64;
        let y_sq = (self.y * self.y) as f64;
        sqrt(x_sq + y_sq)
    }
}

impl Rectangle {
    fn area(&self) -> i32 {
        let width = self.bottom_right.x - self.top_left.x;
        let height = self.bottom_right.y - self.top_left.y;
        width * height
    }
}

fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

fn process_array(arr: [i32; 5]) -> i32 {
    let mut sum = 0;
    for i in 0..5 {
        sum += arr[i];
        if arr[i] < 0 {
            break;
        }
    }
    sum
}

fn main() -> i32 {
    let point = Point::new(3, 4);
    let rect = Rectangle {
        top_left: Point { x: 0, y: 0 },
        bottom_right: Point { x: 10, y: 10 }
    };
    
    let numbers = [1, 2, 3, 4, 5];
    let sum = process_array(numbers);
    
    let status = Status::Ok;
    let result = match status {
        Status::Ok => {
            let fib_10 = fibonacci(10);
            fib_10 + sum
        },
        Status::Error => -1,
        Status::Pending => 0
    };
    
    // Pointer operations
    let x = 42;
    let ptr = &x;
    let value = *ptr;
    
    // Loop with break/continue
    let mut counter = 0;
    loop {
        counter += 1;
        if counter % 2 == 0 {
            continue;
        }
        if counter > 10 {
            break counter;
        }
    }
}
" in

  let success = test_parse "Complete Program" program1 in
  success

let run_all_tests () =
  Printf.printf "🧪 COMPREHENSIVE EIRON COMPILER TEST SUITE\n";
  Printf.printf "==========================================\n";
  Printf.printf "Testing every feature from the EBNF specification...\n";
  
  let results = [
    ("Literals", test_literals ());
    ("Types", test_types ());
    ("Expressions", test_expressions ());
    ("Statements", test_statements ());
    ("Control Flow", test_control_flow ());
    ("Data Structures", test_data_structures ());
    ("Functions", test_functions ());
    ("Advanced Features", test_advanced_features ());
    ("Edge Cases", test_edge_cases ());
    ("Complete Programs", test_complete_programs ());
  ] in
  
  Printf.printf "\n\n📊 TEST RESULTS SUMMARY\n";
  Printf.printf "========================\n";
  
  let (passed, total) = List.fold_left (fun (p, t) (name, success) ->
    Printf.printf "%s: %s\n" name (if success then "✅ PASS" else "❌ FAIL");
    ((if success then p + 1 else p), t + 1)
  ) (0, 0) results in
  
  Printf.printf "\n🎯 Overall Score: %d/%d (%.1f%%)\n" 
    passed total ((float_of_int passed) /. (float_of_int total) *. 100.0);
  
  if passed = total then (
    Printf.printf "\n🎉 ALL TESTS PASSED! Compiler is ready for LLVM integration.\n"
  ) else (
    Printf.printf "\n⚠️  Some tests failed. Need to fix issues before LLVM integration.\n"
  );
  
  passed = total

let () = 
  let success = run_all_tests () in
  exit (if success then 0 else 1)
