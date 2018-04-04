
open Batteries

type t = int

let fresh = unique

let compare = Int.compare

let to_int u = u

let pp = PP.int

let to_string = string_of_int
