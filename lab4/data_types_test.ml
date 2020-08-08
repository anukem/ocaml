open OUnit2
open Data_types

let tests = "test suite for data types" >::: [
  "earliest date in 2020"  >:: (fun _ -> assert_equal (Some (2020, 2, 1)) (earliest [(2020, 2, 1); (2023, 2, 1)]));
  "earliest date is at the end"  >:: (fun _ -> assert_equal (Some (2020, 2, 1)) (earliest [(2023, 2, 1);(2020, 2, 1) ]));
  "earliest date is in the middle"  >:: (fun _ -> assert_equal (Some (1800, 2, 1)) (earliest [(2023, 2, 1); (1800, 2, 1); (2020, 2, 1) ]));
  "is_before an invalid date"  >:: (fun _ -> assert_equal false (is_before (2023, 2, 1) (100, 2342, 1) ));
  "dates are not valid dates, but arbitrary numbers"  >:: (fun _ -> assert_equal (Some (100, 2342, 1)) (earliest [(2020, 3, 1); (1900, 3, 2); (100, 2342, 1)]));
]

let _ = run_test_tt_main tests