open OUnit2
open Higher_order

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

let new_tree =
  Node(5,
    Node(3,
      Node(2,Leaf,Leaf),
      Node(4,Leaf,Leaf)
    ),
    Node(6,
      Node(7,Leaf,Leaf),
      Node(8,Leaf,Leaf)
    )
  )

let my_tree = Node ((4, "four"),
                Node ((2, "two"), Leaf, Leaf),
                Node ((5, "five"), Leaf, Leaf))

let leaf = Leaf

let tests = "test suite for variants" >::: [
  "adds one to every node"          >:: (fun _ -> assert_equal new_tree (add1 same_tree));
]

let _ = run_test_tt_main tests
