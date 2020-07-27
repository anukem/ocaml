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