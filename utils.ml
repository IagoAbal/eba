
open Batteries

let compare_on f x y = Pervasives.compare (f x) (f y)

let compare_first (x,_) (y,_) = Pervasives.compare x y

let compare_on_first f (x,_) (y,_) = Pervasives.compare (f x) (f y)

let equal_on f x y = (f x) = (f y)

let apply_if cond f x =
	if cond
	then f x
	else x

let instr_same_loc = equal_on Cil.get_instrLoc

let match_pair = function
	| [a;b] -> (a,b)
	| ____  -> Error.panic_with "match_pair: not a 2-element list"

let colored cd str = Printf.sprintf "\027[%sm%s\027[0m" cd str

let green = colored "0;32"
let purple = colored "0;35"
let cyan = colored "0;36"

let check_if_file_exists fname :unit =
	if Sys.file_exists fname
	then ()
	else begin
		Printf.eprintf "eba: cannot analyze `%s': File not found\n" fname;
		exit 1
	end

module Option = struct

	let (<|>) opt1 opt2 = fun x ->
		match opt1 x with
		| Some _ as r -> r
		| None        -> opt2 x

	let (=>?) guard v =
		if guard then Some v else None

let fold_left_break ~f y0 xs =
	let open Option.Infix in
	let open Return in
	label (fun break ->
		List.fold_left (fun y_opt x ->
			y_opt >>= fun y ->
			match f y x with
			| None    -> return break None
			| Some y1 -> Some y1
		)
		(Some y0)
		xs
	)

end

let string_of_cil ppr x :string =
	let x_doc = ppr () x in
	Pretty.sprint ~width:60 x_doc

let pp_upto max sep pp_el els =
	let open PP in
	let max_els = Enum.take max els in
	let more_els = not Enum.(is_empty (skip max els)) in
	let els_docs = max_els |> Enum.map pp_el |> List.of_enum in
	let ending =
		if more_els
		then [!^ "..."]
		else []
	in
	separate sep (els_docs @ ending)

let is_zero_arg_proc typ =
	assert (Cil.isFunctionType typ);
	let _, args, varargs, _ = Cil.splitFunctionType typ in
	match args with
	| None
	| Some []
	-> not varargs
	| _else
	-> false

module Varinfo =
struct

	open Cil

	type t = varinfo

	let vid x = x.vid

	let compare x y = Int.compare x.vid y.vid

	let equal x y = compare x y = 0

	let hash = Hashtbl.hash

	let loc_of x = x.vdecl

end

module Exp =
struct

	type t = Cil.exp

	(* Convert from CIL's Pretty.doc to our PP.doc *)
	let pp e :PP.doc =
		let open Cil in
		let e_doc =
			match e with
			| Lval (Var x,NoOffset)
			(* HACK to recognize CIL temporary variables *)
			when x.vname = "tmp"
			|| String.starts_with x.vname "tmp___" ->
				x.vdescr
			| _else ->
				Cil.d_exp () e
		in
		let e_str = Pretty.sprint ~width:60 e_doc in
		PP.(!^ e_str)

	let to_string = PP.to_string % pp

end

module Location =
struct

	open Tuple

	type t = Cil.location

	let compare = Cil.compareLoc

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
