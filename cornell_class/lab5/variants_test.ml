open OUnit2
open Variants

let tree =
  Node((4, "four"),
    Node((2, "two"),
      Node((1, "one"),Leaf,Leaf),
      Node((3, "three"),Leaf,Leaf)
    ),
    Node((5, "five"),
      Node((6, "six"),Leaf,Leaf),
      Node((7, "seven"),Leaf,Leaf)
    )
  )


let same_tree =
  Node(4,
    Node(2,
      Node(1,Leaf,Leaf),
      Node(3,Leaf,Leaf)
    ),
    Node(5,
      Node(6,Leaf,Leaf),
      Node(7,Leaf,Leaf)
    )
  )

let my_tree = Node ((4, "four"),
                Node ((2, "two"), Leaf, Leaf),
                Node ((5, "five"), Leaf, Leaf))


let bad_tree = Node ((4, "four"),
                Node ((6, "two"), Leaf, Leaf),
                Node ((298, "five"), Leaf, Leaf))

let other_bad_tree = Node ((4, "four"),
                Node ((1, "two"), Leaf, Leaf),
                Node ((1, "five"), Leaf, Leaf))

let leaf = Leaf

let tests = "test suite for variants" >::: [
  "check the tree against itself"                        >:: (fun _ -> assert_equal true (same_shape tree tree));
  "different values, same tree shape"                    >:: (fun _ -> assert_equal true (same_shape same_tree tree));
  "different trees altogether"                           >:: (fun _ -> assert_equal false (same_shape leaf tree));
  "it calculates max correctly"                          >:: (fun _ -> assert_equal 7 (maximium tree));
  "it calculates min correctly"                          >:: (fun _ -> assert_equal 1 (minimium tree));
  "is_bst succeeds on valid input"                       >:: (fun _ -> assert_equal true (is_bst my_tree));
  "is_bst fails on invalid input"                        >:: (fun _ -> assert_equal false (is_bst bad_tree));
  "is_bst fails on invalid input when right is greater"  >:: (fun _ -> assert_equal false (is_bst bad_tree));
  "int_max_string throws exception on empty list"        >:: (fun _ -> assert_equal "empty" (int_max_string []));
  "int_max_string throws exception on empty list"        >:: (fun _ -> assert_raises (Failure "int_max failure") (fun x -> int_max []));
]

let _ = run_test_tt_main tests
