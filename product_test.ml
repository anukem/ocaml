open OUnit2
open Mul 

let tests = "test suite for multiplying" >::: [
  "one and one"           >:: (fun _ -> assert_equal 1 (mul [1;1]));
  "adding seven and one"  >:: (fun _ -> assert_equal 6 (mul [6;1]));
]

let _ = run_test_tt_main tests
