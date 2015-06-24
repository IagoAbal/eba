
open Batteries

open Type

module L = LazyList

module Spec = struct
	let name = "Flow-2 double-lock checker"

	type st = Region.t

	let select fileAbs fd fsch fnAbs =
		let feffects = E.principal (FunAbs.sum fnAbs) in
		let locked = E.(regions(filter is_locks feffects)) in
		L.of_enum (Regions.enum locked)

	let may_unlock r ef :bool =	E.(mem (unlocks r) (principal ef))

	let not_unlocks r ef :bool = not (may_unlock r ef)

	let locks r ef :bool = E.(mem_must (locks r) (principal ef))

	let locks_and_not_unlocks r ef = locks r ef && not_unlocks r ef

	let testP1 _ _ = true

	let testQ1 = locks_and_not_unlocks

	let testP2 = not_unlocks

	let testQ2 = locks

	type bug = Region.t

	let bug_of_st r = r

	let doc_of_report ~fn r ~loc1 ~loc2 ~trace =
		let open PP in
		brackets (!^ name) + newline +
		words "Double lock" ++ parens(Region.pp r) + newline
		++ words "first at" ++ (Utils.Location.pp loc1) + newline
		++ words "second at" ++ (Utils.Location.pp loc2) + newline
		+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
		++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
		+ PathTree.pp_path trace

end

module Checker = Flow2Checker.Make(Spec)

include Checker
