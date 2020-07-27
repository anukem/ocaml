type student = { first_name : string ; last_name : string ; gpa : float }
type poketype = Normal | Fire | Water
type pokemon = { name: string; hp: int; ptype: poketype; }

let ezeugo = { first_name = "John" ; last_name = "Anukem"; gpa = 3.1 }

let fullName s = s.first_name, s.last_name

let createStudent fir las g = {first_name = fir; last_name = las; gpa = g}

let safe_hd lst x = 
  match lst with 
    | [] -> None
    | h::tl -> if h = x then (Some x) else None

let safe_tail lst x = 
  let listRev = List.rev lst in
  let tail = List.hd listRev in
  match lst with 
    | [] -> None
    | h::tl -> if tail = x then (Some x) else None


let rec max_hp pk_list = 
  match pk_list with 
  | [] -> None
  | h::tl -> let newHp = max_hp tl in if newHp > Some h.hp then newHp else Some h.hp

let is_before (x, y, z) (a, b, c) = 
  if x < a then true else
  if y < b && x = a then true else
  if z < c && y = b && x = a then true else false

let rec earliest date_list = 
  match date_list with 
  | [] -> None
  | h::[] -> Some h
  | h::tl -> let e = earliest tl in
  if is_before h (match e with | Some e -> e | None -> (0,0,0)) then Some h else earliest tl

(* insert a binding from key k to value v in association list d *)
let insert k v d = (k,v)::d

(* find the value v to which key k is bound, if any, in the assocation list *)
let rec lookup k = function
| [] -> None
| (kay,v)::t -> if k = kay then Some v else lookup k t

let association = insert 1 "one" [] @ insert 2 "two" []
