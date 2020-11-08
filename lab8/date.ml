type date = { month:int; day:int }

module Date = struct
  type t = date
  
  let compare d1 d2 = match d1.month = d2.month with
    | true -> d2.day - d1.day
    | false -> d2.month - d1.month
end


module DateMap = Map.Make(Date)
type calendar = string DateMap.t

let my_calendar = DateMap.empty

let print_calendar c = DateMap.iter (fun key name -> print_endline ((string_of_int key.month) ^ "/" ^ (string_of_int key.day) ^ ": " ^ name)) c
