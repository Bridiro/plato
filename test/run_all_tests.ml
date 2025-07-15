(* Test Runner - Runs all tests and reports results for CI *)

let run_test_executable name =
  let cmd =
    Printf.sprintf "cd %s && ./_build/default/test/%s.exe" (Sys.getcwd ()) name
  in
  let exit_code = Sys.command cmd in
  (name, exit_code = 0)

let run_all_tests () =
  Printf.printf "=== EIRON LANGUAGE TEST SUITE ===\n\n" ;

  let tests =
    [
      "test_lexer";
      "test_parser";
      "test_type_checker";
      "test_integration";
      "test_features";
      "test_comprehensive";
    ]
  in

  let results = List.map run_test_executable tests in

  Printf.printf "\n=== TEST RESULTS SUMMARY ===\n" ;
  let passed = ref 0 in
  let total = List.length results in

  List.iter
    (fun (name, success) ->
      if success then (
        Printf.printf "✓ %s PASSED\n" name ;
        incr passed)
      else
        Printf.printf "✗ %s FAILED\n" name)
    results ;

  Printf.printf "\nOverall: %d/%d tests passed\n" !passed total ;

  if !passed = total then (
    Printf.printf "\n🎉 All tests passed! The build is ready.\n" ;
    exit 0)
  else (
    Printf.printf "\n❌ Some tests failed. Build failed.\n" ;
    exit 1)

let () = run_all_tests ()
