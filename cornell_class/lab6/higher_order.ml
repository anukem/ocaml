let double x = 2 * x

let square x = x * x

let cube x = x * x * x

let twice f x = f (f x)

let quad = twice double

let fourth = twice square

let pipeline x f = f x

let ( |> ) = pipeline

let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *)
let ( -- ) i j = from i j []

let rec repeat n f x = if n = 1 then f x else f (repeat (n - 1) f x)

let product_left l = List.fold_left ( *. ) 1.0 l

let product_right l = List.fold_right ( *. ) l 1.0

let is_odd n = n mod 2 <> 0

let sum_cube_odd n =
  List.fold_left ( + ) 0
    (List.map cube (List.filter (fun x -> x mod 2 <> 0) (0 -- n)))

let sum_cube_odd_pipe n =
  0 -- n |> List.filter is_odd |> List.map cube |> List.fold_left ( + ) 0

let rec exists_rec p lst =
  match lst with [] -> false | h :: tl -> p h || exists_rec p tl

let exists_fold p lst = List.fold_left (fun x y -> x || p y) false lst

let exists_lib p lst = List.map p lst |> List.exists (fun x -> x = true)

let uncurry f (a, b) = f a b

let uncurried_nth (a : 'a) (b : 'b) = uncurry List.nth

let uncurried_append (a : 'a) (b : 'b) = uncurry List.append

let uncurried_compare = uncurry Char.compare

let uncurried_max (a : 'a) (b : 'b) = uncurry max

let curry f a b = f (a, b)

let map_comp f g lst = List.map (fun x -> f g x) lst

let greater_than_three lst = List.filter (fun x -> x > 3) lst

let add_one lst = List.map (fun x -> x +. 1.) lst

let combine_strs lst sep =
  match lst with
  | h :: tl -> List.fold_left (fun x y -> x ^ sep ^ y) h tl
  | [] -> sep

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec map_tree t f =
  match t with
  | Leaf -> Leaf
  | Node (v, left, right) -> Node (f v, map_tree left f, map_tree right f)

let add1 t = map_tree t (fun x -> x + 1)

let keys lst = List.map fst lst |> List.sort_uniq compare
