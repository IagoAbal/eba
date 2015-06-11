(* THINK: Split into:
 * - File level, for globals;
 * - Function level, for locals.
 *)

open Batteries

open Type

module L = LazyList

module Spec = struct
	type bug = Region.t

	let name = "FI use before initialization"

	let string_of_report fd r =
		let fn = Cil.(fd.svar) in
		let open Cil in
		let open Pretty in
		gprintf (sprint ~width:75)
			"Read of uninitialized variable %s:\n\
			In %s at %a"
			(Region.to_string r) fn.vname d_loc fn.vdecl

	let test fileAbs fd fnAbs fsch =
		let feffects = Scheme.effects_of_fun fsch in
		let geffects = FileAbs.sum fileAbs in
		let lregions = FunAbs.regions_of_list fnAbs Cil.(fd.slocals) in
		(* We filter out regions that are only accessible via
		 * functions' arguments, since those may or may not be
		 * initialized depending on the calling context. *)
		let target r = Region.is_meta r || Regions.mem r lregions in
		feffects |> Enum.filter_map (function
		| E.Must(E.Mem(E.Read,r)) when target r ->
			let write_r = E.writes r in
			if not(E.mem write_r geffects)
			then Some r
			else None
		| __other__ -> None
		) |> L.of_enum
end

module Checker = FiChecker.Make(Spec)

include Checker
