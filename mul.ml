let rec mul lst = 
  match lst with 
  | [] -> 1
  | h::t -> h * mul t