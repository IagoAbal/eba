
open Batteries

let compare_on f x y = Pervasives.compare (f x) (f y)

let equal_on f x y = (f x) = (f y)

let instr_same_loc = equal_on Cil.get_instrLoc

let match_pair = function
	| [a;b] -> (a,b)
	| ____  -> Error.panic_with "match_pair: not a 2-element list"

module Varinfo =
struct

	type t = Cil.varinfo

	let vid x = Cil.(x.vid)

	let compare x y = Pervasives.compare (vid x) (vid y)

	let equal x y = compare x y = 0

	let hash = Hashtbl.hash

end

module Exp =
struct

	type t = Cil.exp

	(* Convert from CIL's Pretty.doc to our PP.doc *)
	let pp e :PP.doc =
		let e_doc = Cil.d_exp () e in
		let e_str = Pretty.sprint ~width:60 e_doc in
		PP.(!^ e_str)

	let to_string = PP.to_string % pp

end

module Location =
struct

	open Tuple

	type t = Cil.location

	let compare = Pervasives.compare

	let equal x y = compare x y = 0

	let hash = Hashtbl.hash

	(* Convert from CIL's Pretty.doc to our PP.doc *)
	let pp l :PP.doc =
		let l_doc = Cil.d_loc () l in
		let l_str = Pretty.sprint ~width:60 l_doc in
		PP.(!^ l_str)

	let to_string = PP.to_string % pp

	let pp_with_loc :(Cil.location * PP.doc) list -> PP.doc =
		let with_loc (loc,x_pp) = PP.(pp loc ++ x_pp) in
		PP.separate PP.newline %
			List.map with_loc %
			List.sort (compare_on Tuple2.first)

end
