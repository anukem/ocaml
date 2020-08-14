let rec print_int_list lst =
  match lst with
  | [] -> ()
  | h::t -> print_int h; print_char '\n'; print_int_list t

let print_int_list' lst = List.iter (fun x -> (print_int x; print_char '\n')) lst
