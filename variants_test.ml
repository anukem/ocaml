open OUnit2
open Variants

let tree =
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

let leaf = Leaf

let tests = "test suite for variants" >::: [
  "check the tree against itself"     >:: (fun _ -> assert_equal true (same_shape tree tree));
  "different values, same tree shape" >:: (fun _ -> assert_equal true (same_shape same_tree tree));
  "different trees altogether"        >:: (fun _ -> assert_equal false (same_shape leaf tree));
]

let _ = run_test_tt_main tests