open OUnit2
open Lst 

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let tests = "test suite for lists" >::: [
  "list drops two things"  >:: (fun _ -> assert_equal [3;4;5] (drop 2 [1;2;3;4;5]) );
]

let _ = run_test_tt_main tests