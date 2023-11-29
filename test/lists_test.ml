open Ocaml_exercises
open OUnit2

let last_tests = "test suite for last" >::: [
  "multi" >:: (
    fun _ -> match Lists.last ["a"; "b"; "c"; "d"] with 
    | Some x -> assert_equal x "d"
    | None -> assert_failure "None was found as the last element where 'd' was expected"
  );
  "singleton" >:: (
    fun _ -> match Lists.last ["a"] with 
    | Some x -> assert_equal x "a"
    | None -> assert_failure "None was found as the last element where 'a' was expected"
  );
  "empty" >:: (fun _ -> assert_equal None (Lists.last []));
]

let _ = run_test_tt_main last_tests
