let rec h n pp p = if n = 1 then p else h (n-1) p (pp + p)

let fib n =
  if 0 = n then 0 else h n 0 1

let id x = x

let divide ~numerator:n ~demonimator:d = n /. d
let rec concat lst =
  match lst with 
  | [] -> ""
  | h::t -> h ^ concat t