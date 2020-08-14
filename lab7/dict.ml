module type Dictionary = sig
  type ('k, 'v) t

  (* The empty dictionary *)
  val empty  : ('k, 'v) t

  (* [insert k v d] produces a new dictionary [d'] with the same mappings
   * as [d] and also a mapping from [k] to [v], even if [k] was already
   * mapped in [d]. *)
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t

  (* [lookup k d] returns the value associated with [k] in [d].
   * raises:  [Not_found] if [k] is not mapped to any value in [d]. *)
  val lookup  : 'k -> ('k,'v) t -> 'v
end

module AssocListDict : Dictionary = struct
  type ('k, 'v) t = ('k * 'v) list

  let empty = []

  let insert k v d = (k, v)::empty

  let lookup k d = List.assoc k d
end

module BstDict : Dictionary = struct
  type ('k, 'v) t = ('k * 'v) Variants.tree

  let empty = Variants.Leaf
  let insert k v d = let open Variants in insert k v d
  let lookup k d = let open Variants in
    match lookup k d with
    | None -> failwith "no value found"
    | Some x -> x
end

type frac = Frac of int * int

module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator   : t -> int
  val denominator : t -> int
  val toString    : t -> string
  val toReal      : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

(* [gcd x y] is the greatest common divisor of [x] and [y].
 * requires: [x] and [y] are positive.
 *)
let rec gcd (x:int) (y:int) : int =
  if x = 0 then y
  else if (x < y) then gcd (y - x) x
  else gcd y (x - y)

let reduce f =
  match f with
  | Frac (n, d) -> let deno = gcd n d in Frac (n / deno, d / deno)

module FractionPair : Fraction = struct
  type t = frac

  let make n d = Frac (n, d)

  let numerator f =  match f with Frac (n, d) -> n
  let denominator f = match f with Frac (n, d) -> d
  let toString f = match f with
    | Frac (n, d) -> (string_of_int n) ^ "/" ^ (string_of_int d)
  let toReal f = match f with Frac (n, d) -> (float_of_int n) /. (float_of_int d)

  let add f1 f2 =
    match f1, f2 with Frac (n1, d1), Frac (n2, d2) -> let new_deno = d1 * d2 in
    reduce (Frac ((n1 * d2) + (n2 * d1), new_deno))

  let mul f1 f2 = match f1, f2 with Frac (n1, d1), Frac (n2, d2) -> Frac ((n1 * n2), (d1 * d2)) |> reduce
end
