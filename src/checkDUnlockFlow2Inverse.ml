
open Batteries

open Type
open Abs
open PathTree

open Utils.Option

module CE = CilExtra

module L = LazyList

module Spec = struct
	let name = "Flow-2 double-unlock checker - inverse"

	type st = {
		fna  : AFun.t;
		reg  : Region.t;
		unlock : Cil.exp option;
		kreg : Regions.t;
	}

	let init_st fna r = { fna; reg = r; unlock = None; kreg = Regions.empty; }

	let select _fla _ _ fna =
		let feffects = AFun.sum fna in
		let unlocked = E.(regions(filter is_unlocks feffects)) in
		L.of_enum (Enum.map (init_st fna) (Regions.enum unlocked))

	let trace st ef =
		let ef_rs = E.(regions (filter (not % is_reads) ef)) in
		Regions.(mem st.reg ef_rs)

	let not_locks r ef :bool = not E.(mem_must (locks ~r) ef)

	let find_unlock_object = find_in_stmt CE.find_linux_lock_in_call
	
	let testP1 st _ = Some st

	let testQ1 st step =
		let unlocks_and_not_locks r ef = 
			let unlocks r ef :bool = E.(mem_must (unlocks ~r) ef) in
			unlocks r ef && not_locks r ef in

		unlocks_and_not_locks st.reg step.effs =>?
			let unlock_opt = find_unlock_object step in
			let krs = Option.Infix.(
				(unlock_opt >>= Lenv.kregions_of st.fna)
				|? Regions.empty
			) in
			{st with unlock = unlock_opt; kreg = krs}

	let testP2 st step =
		not_locks st.reg step.effs =>? st

	let testQ2_weak st step =
		let must_unlock = E.(mem (unlocks ~r:st.reg) step.effs) in
		if must_unlock
		then
			(* let unlock_obj = find_unlock_object step in
			match st.unlock, unlock_obj with
			| Some lo1, Some lo2
			when Opts.match_unlock_exp ()
			  (* If this step satisfies "Q2-strong". *)
			  && Option.is_some (testP2 st step)
			  (* Then, we compare the two lock object expressions involved
			   * and heuristically determine if, despite aliasing information,
			   * may not denote the same lock object.
			   *)
			  (* && not (CE.equal_offset lo1 lo2) *)
			  -> None
			| _, _ -> Some st *)
			Some st
		else None

	type bug = Region.t

	let bug_of_st st = st.reg

	let doc_of_report ~fn r ~loc1 ~loc2 ~trace =
		let open PP in
		brackets (!^ name) + newline +
		words "Double unlock" ++ parens(Region.pp r) + newline
		++ words "first at" ++ (Utils.Location.pp loc1) + newline
		++ words "second at" ++ (Utils.Location.pp loc2) + newline
		+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
		++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
		+ PathTree.pp_path trace

end

module Checker = Flow2Checker.Make(Spec)

include Checker
