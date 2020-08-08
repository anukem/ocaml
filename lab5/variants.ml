type suit = Spade | Heart | Club | Diamond

type rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

type card = { rank: rank; suit: suit; }

let ace_of_clubs = { rank = Ace; suit = Club }

let queen_of_hearts = { rank = Queen; suit = Heart }

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign = if x < 0 then Neg else if x = 0 then Zero else Pos
let new_sign x = if x < 0 then `Neg else if x = 0 then `Zero else `Pos

let quadrant : int*int -> quad option = fun (x,y) ->
  match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IV
    | Zero, _ -> None
    | _, Zero -> None

let new_quadrant (x, y) =
  match new_sign x, new_sign y with
    | `Pos, `Pos -> Some `I
    | `Neg, `Pos -> Some `II
    | `Neg, `Neg -> Some `III
    | `Pos, `Neg -> Some `IV
    | `Zero, _ -> None
    | _, `Zero -> None

let quadrant_when (x, y) =
  match x, y with
  | x,y when x > 0 && y > 0 -> Some I
  | x,y when x < 0 && y > 0 -> Some II
  | x,y when x < 0 && y < 0 -> Some III
  | x,y when x > 0 && y < 0 -> Some IV
  | _,_ -> None

type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree

let rec depth : 'a tree -> int = fun tree ->
  match tree with
    | Leaf -> 0
    | Node (_, left, right) -> 1 + max (depth left) (depth right)
  
let rec same_shape t1 t2 = 
  match t1, t2 with
  | Node (_, l1, r1), Node (_,l2, r2) -> (same_shape l1 l2) = true && (same_shape r1 r2) = true
  | Leaf, Leaf -> true
  | _, _ -> false

let rec int_max int_list =
  match int_list with 
  | [] -> failwith "int_max failure"
  | h::[] -> h
  | h::tl -> max h (int_max tl)

let rec int_max_string int_list = 
  match int_max int_list with
    | s -> string_of_int s
    | exception (Failure s) -> "empty"

let rec lookup key tree = 
  match tree with 
  | Node ((k, v), left, right) when key = k -> Some v
  | Node ((k, v), left, right) when key > k -> lookup key right
  | Node ((k, v), left, right) when key < k -> lookup key left
  | Node (_, left, right) -> failwith "Not a valid node"
  | Leaf -> None

let rec insert key value tree =
  match tree with
  | Node ((k, v), left, right) when key > k -> Node ((k, v), left, (insert key value right)) 
  | Node ((k, v), left, right) when key < k -> Node ((k, v), (insert key value left), right)
  | Node ((k, v), left, right) when key = k -> Node ((key, value), left, right) 
  | Node (_, left, right) -> failwith "Not a valid node"
  | Leaf -> Node ((key, value), Leaf, Leaf)

let rec minimium tree = 
  match tree with 
  | Leaf -> failwith "empty tree"
  | Node ((k, v), Leaf, Leaf) -> k
  | Node ((k, v), Node ((a, b), c, d), Leaf) -> min k (minimium (Node ((a, b), c, d)))
  | Node ((k, v),Leaf, Node ((a, b), c, d)) -> min k (minimium (Node ((a, b), c, d)))
  | Node ((k, v), Node ((e, f), g, h), Node ((a, b), c, d)) -> min (minimium (Node ((e, f), g, h))) (min k (minimium (Node ((a, b), c, d))))


let rec maximium tree = 
  match tree with 
  | Leaf -> failwith "empty tree"
  | Node ((k, v), Leaf, Leaf) -> k
  | Node ((k, v), Node ((a, b), c, d), Leaf) -> max k (maximium (Node ((a, b), c, d)))
  | Node ((k, v),Leaf, Node ((a, b), c, d)) -> max k (maximium (Node ((a, b), c, d)))
  | Node ((k, v), Node ((e, f), g, h), Node ((a, b), c, d)) -> max (maximium (Node ((e, f), g, h))) (max k (maximium (Node ((a, b), c, d))))


let is_bst tree = 
  let _max = maximium tree in
  let _min = minimium tree in 
  let rec is_valid_bst high low tree = 
    match tree with
    | Leaf -> true
    | Node ((k, v), left, right) -> if k >= low && k <= high then is_valid_bst k _min left && is_valid_bst _max k right else false

  in is_valid_bst _max _min tree

