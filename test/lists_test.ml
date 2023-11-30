open Ocaml_exercises
open OUnit2

let last_tests = "test suite for Lists.last" >::: [
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

let last_two_tests = "test suite for Lists.last_two" >::: [
  "double" >:: (
    fun _ -> match Lists.last_two ["a"; "b"] with
      | None -> assert_failure "None was found as the last two elements where 'a', 'b' was expected"
    | Some (x,y) -> assert_equal (x, y) ("a", "b")
  );
  "singleton" >:: (
    fun _ -> match Lists.last_two ["a"] with 
    | Some _ -> assert_failure "None was expected"
    | None -> ()
  );
  "empty" >:: (fun _ -> assert_equal None (Lists.last_two []));
]

let _ = run_test_tt_main last_two_tests

let nth_tests = "test suite for Lists.nth" >::: [
  "empty" >:: (fun _ -> assert_equal None (Lists.nth [] 1));
  "singleton" >:: (
    fun _ -> match Lists.nth ["a"] 0 with
    | Some x -> assert_equal x "a"
    | None -> assert_failure "None was found, 'a' was expected"
  );
  "non-empty" >:: (
    fun _ -> match Lists.nth ["a"; "b"; "c"; "d"; "e"] 3 with
    | Some x -> assert_equal x "d"
    | None -> assert_failure "None was found, 'd' was expected"
  );
  "out of bounds" >:: (
    fun _ -> match Lists.nth ["a"; "b"] 3 with
    | Some _ -> assert_failure "Some x was found, None was expected"
    | None -> ()
  )
]

let _ = run_test_tt_main nth_tests

let length_tests = "test suite for Lists.length" >::: [
  "empty" >:: (fun _ -> assert_equal (Lists.length []) 0);
  "non-empty" >:: (fun _ -> assert_equal (Lists.length ["a"; "b"; "c"]) 3)
]

let _ = run_test_tt_main length_tests

let rev_tests = "test suite for Lists.rev" >::: [
  "empty" >:: (fun _ -> assert_equal (Lists.rev []) []);
  "non-empty" >:: (fun _ -> assert_bool "the reverse list was not equal" (Lists.equal (Lists.rev ["a"; "b"; "c"]) ["c"; "b"; "a"]));
]

let _ = run_test_tt_main rev_tests

let palindrome_tests = "test suite for Lists.is_palindrome" >::: [
  "empty" >:: (fun _ -> assert_equal (Lists.is_palindrome []) true);
  "non-empty palindrome" >:: (fun _ -> assert_equal (Lists.is_palindrome [1; 2; 1]) true);
  "non-empty non-palindrome" >:: (fun _ -> assert_equal (Lists.is_palindrome [1; 2; 3; 1]) false);
]

let _ = run_test_tt_main palindrome_tests

let flatten_tests = "test suite for Lists.flatten" >::: [
  "empty" >:: (fun _ -> assert_equal (Lists.flatten []) []);
  "non-empty" >:: (fun _ -> assert_bool "the flattened list was not equal" (
    Lists.equal (Lists.flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]) ["a"; "b"; "c"; "d"; "e"]
  ))
]

let _ = run_test_tt_main flatten_tests

let compress_tests = "test suite for Lists.compress" >::: [
  "empty" >:: (fun _ -> assert_equal (Lists.compress []) []);
  "non-empty" >:: (fun _ -> assert_bool "the compressed list was not equal" (Lists.equal (Lists.compress ["a"; "a"; "b"; "c"; "c"; "d"]) ["a"; "b"; "c"; "d"]))
]

let _ = run_test_tt_main compress_tests
