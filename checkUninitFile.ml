(* This checker will make more sense once we keep track of the locations
 * where bugs occur, so that we do not need to iterate over functions in
 * order to locate the reads.
 *)

open Batteries

open Type

module L = LazyList

module Spec = struct

	(* TODO: Region.t * Cil.fundec list or else remove "duplicates". *)
	type bug = Regions.t * Cil.fundec

	let name = "FI-global use before initialization"

	let string_of_report _ (rs,fd) =
		let fn = Cil.(fd.svar) in
		let open Cil in
		let open Pretty in
		Printf.sprintf "[%s]\n" name
		^ String.concat "\n" (rs |> Regions.to_list |> List.map (fun r ->
			Printf.sprintf "Read of uninitialized variable %s:\n" (Region.to_string r)
		))
		^ gprintf (sprint ~width:75) "In %s at %a" fn.vname d_loc fn.vdecl

	let test file fileAbs =
		let gregions, gveffects = FileAbs.gvar_regions fileAbs in
		let geffects = FileAbs.sum fileAbs in
		let target = Regions.filter (fun r ->
			E.(mem (uninits r) gveffects && not (mem (writes r) geffects))
		) gregions
		in
		let bugs = Cil.(file.globals) |> List.filter_map (function
			| Cil.GFun(fd,_) ->
				let ff = FileAbs.effect_of fileAbs Cil.(fd.svar) in
				let reads =
					Regions.(filter E.(fun r -> mem (reads r) ff) target)
				in
				if Regions.is_empty reads
				then None
				else Some(reads,fd)
			| ______________ -> None
		)
		in
		L.of_list bugs

end

module Checker = FileChecker.Make(Spec)

include Checker
