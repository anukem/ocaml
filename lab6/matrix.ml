let is_empty_matrix m = List.map List.length m |> List.exists (fun x -> x = 0) |> not

let same_length_rows m =
  let int_list = List.map List.length m in
    List.for_all (fun x -> x = (List.hd int_list)) int_list

let is_valid_matrix m = is_empty_matrix m && same_length_rows m


let add_row_vectors lst1 lst2 = if List.length lst1 <> List.length lst2 then [] else List.map2 (+) lst1 lst2

let add_matrices m1 m2 = if List.length m1 <> List.length m2 then [] else List.map2 (add_row_vectors) m1 m2

let dot_product x y = if List.length x <> List.length y then failwith "vectors should be the same dimension" else List.map2 ( * ) x y |> List.fold_left (+) 0

let nth_column n m = List.map (fun x -> List.nth x n) m

let indicies lst = List.init ((List.length (List.hd lst))) (fun x -> x)

let matrix_multiplication m1 m2 =
  if List.length (List.hd m1) <> List.length m2
  then failwith "bad dimensions for the vectors"
  else let get_row index = indicies m2 |> List.map (fun z -> nth_column z m2) |> List.map (fun x -> dot_product x (List.nth m1 index)) in
List.init (List.length m1) (fun x -> x) |> List.map get_row

