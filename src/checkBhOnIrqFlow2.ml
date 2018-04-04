
open Batteries

open Type
open Abs
open PathTree

open Utils.Option

module L = LazyList

module Spec = struct
	let name = "Flow-2 BH-enable when IRQs are off checker"

	type st = unit

	let select fileAbs fd fsch fnAbs =
		let feffects = AFun.sum fnAbs in
		if E.(mem IrqsOff feffects && mem BhsOn feffects)
		then L.(cons () nil)
		else L.nil

	let trace _ _ = false

	let testP1 _ _ = Some ()

	let testQ1 _ step =
		(E.(mem_must IrqsOff step.effs) && not E.(mem IrqsOn step.effs)) =>? ()

	let testP2 _ step = not E.(mem IrqsOn step.effs) =>? ()

	let testQ2_weak _ step = E.(mem BhsOn step.effs) =>? ()

	type bug = unit

	let bug_of_st () = ()

	let doc_of_report ~fn () ~loc1 ~loc2 ~trace =
		let open PP in
		brackets (!^ name) + newline +
		words "Bottom-halves may be enabled with IRQs disabled" + newline
		++ words "IRQs off at" ++ (Utils.Location.pp loc1) + newline
		++ words "BHs on at" ++ (Utils.Location.pp loc2) + newline
		+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
		++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
		+ PathTree.pp_path trace

end

module Checker = Flow2Checker.Make(Spec)

include Checker
