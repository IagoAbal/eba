
open Batteries

type t = int

let fresh = unique

let compare = compare

let to_int u = u

let pp = PP.int

let to_string = string_of_int
