
open Batteries

type t = int

let fresh = unique

let compare = compare

let pp = PP.int

let to_string = string_of_int
