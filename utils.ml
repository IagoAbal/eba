
open Batteries

let compare_on f x y = Pervasives.compare (f x) (f y)

