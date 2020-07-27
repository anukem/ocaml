let bigred lst = 
  match lst with
  | [] -> false
  | h::t -> if h = "bigred" then true else false

let exactlyTwoOrFour lst = 
  match lst with
  | [] -> false
  | h::[] -> false
  | h::a::[] -> true
  | h::a::b::[] -> false
  | h::a::b::c::[] -> true
  | _ -> false

let firstTwoEqual lst =
  match lst with
  | [] -> false
  | h::[] -> false
  | h::a::t -> h = a
  
let fifthElement lst = 
  match lst with 
  | [] -> 0
  | h::t -> if List.length t < 4 then 0 else List.nth t 3

let descendingSort lst = List.rev (List.sort Stdlib.compare lst)

let lastElement lst = List.hd (List.rev lst)

let any_zeroes lst = List.exists (fun x -> x = 0) lst

let rec take n lst =
  match lst with
  | [] -> []
  | h::tl -> if n = 1 then [h] else h::(take (n-1) tl)

let rec drop n lst =
  match lst with
  | [] -> []
  | h::tl -> if n = 0 then lst else drop (n-1) tl

let rec take_fast n lst =
  let listRev = List.rev lst in
  match lst with
  | [] -> []
  | h::t -> List.rev_map (fun x -> x) (drop ((List.length listRev) - n) listRev)

  (* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l =
  if i>j then l
  else from i (j-1) (j::l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *) 
let (--) i j =
  from i j []

let longlist = 0 -- 1_000_000