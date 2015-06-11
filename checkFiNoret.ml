
open Batteries

open Type

module L = LazyList

module Spec = struct
	type bug = unit

	let name = "FI undeclared non-returning function"

	let string_of_report fd r =
		let fn = Cil.(fd.svar) in
		let open Cil in
		let open Pretty in
		gprintf (sprint ~width:75)
			"[%s]\n\
			This function will never return\n\
			but is missing the `noreturn' attribute.\n\
			In %s at %a"
			name fn.vname d_loc fn.vdecl

	let test fileAbs fd fnAbs fsch =
		let ef = FileAbs.effect_of fileAbs Cil.(fd.svar) in
		let noret = E.(mem_must noret ef) in
		let has_noret_attr = Cil.(hasAttribute "noreturn" fd.svar.vattr) in
		if noret && not has_noret_attr
		then L.cons () L.nil
		else L.nil
end

module Checker = FiChecker.Make(Spec)

include Checker
