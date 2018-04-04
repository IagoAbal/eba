
open Batteries

open Type
open Abs
open PathTree

open Utils.Option

module L = LazyList

module Spec = struct

	type st = Region.t
	type bug = Region.t

	let name = "Flow-1 use-before-initialization local-variable checker"

	let bug_of_st r = r

	let doc_of_bug r =
		PP.(!^ "Use before initialization" ++ parens(Region.pp r))

 	let select fileAbs fd fsch fnAbs =
		let luninit = AFun.uninit_locals fnAbs fd in
 		let feffects = AFun.sum fnAbs in
		let read = E.(regions(filter is_reads feffects)) in
		let target = Regions.(inter luninit read) in
		(* TODO: Define list : Regions.t -> Region.t LazyList.t *)
		L.of_enum (Regions.enum target)

	let trace r ef = Regions.mem r E.(regions ef)

	let may_write r ef :bool = E.(mem (writes r) ef)

	let not_writes r ef :bool = not (may_write r ef)

	let reads r ef :bool = E.(mem_must (reads r) ef)

	let reads_and_not_writes r ef = reads r ef && not_writes r ef

	let testP r step = not_writes r step.effs =>? r

	let testQ r step = reads_and_not_writes r step.effs =>? r
end

module Checker = Flow1Checker.Make(Spec)

include Checker
