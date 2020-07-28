type suit = Spade | Heart | Club | Diamond

type rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

type card = { rank: rank; suit: suit; }

let ace_of_clubs = { rank = Ace; suit = Club }

let queen_of_hearts = { rank = Queen; suit = Heart }

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign = if x < 0 then Neg else if x = 0 then Zero else Pos

let quadrant : int*int -> quad option = fun (x,y) ->
  match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IV
    | Zero, _ -> None
    | _, Zero -> None

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