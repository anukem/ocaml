open OUnit2
open Add

let tests = "test suite for adding" >::: [
  "one and one"           >:: (fun _ -> assert_equal 2 (add 1 1));
  "adding seven and one"  >:: (fun _ -> assert_equal 7 (add 6 1));
]

let _ = run_test_tt_main tests