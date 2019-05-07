(***

========================   Double unlock checker  ===========================

Based on double lock checker: checkDLockFlow2.ml

Sumary of changes per line:

Line #    Change
28        is_locks --> is_unlocks ; locked -> unlocked
29        locked --> unlocked
51        change may_lock to line 35 (above may_unlock)
39        not_unlocks --> not_locks ; may_unlock --> may_lock
51        locks --> unlocks (x2)
53        locks_and_not_unlocks --> unlocks_and_not_locks ; locks --> unlocks ; not_unlocks --> not_locks
60        locks_and_not_unlocks --> unlocks_and_not_locks
80        not_unlocks --> not_locks
84        may_lock --> may_unlock
110       "lock" --> "unlock" (strings)


May change??
59-60   Change anything??

 ***)




open Batteries

open Type
open Abs
open PathTree

open Utils.Option

module CE = CilExtra

module L = LazyList

module Spec = struct
	let name = "Flow-2 double-unlock checker"

	type st = {
		fna  : AFun.t;
		reg  : Region.t;
		lock : Cil.exp option;
		kreg : Regions.t;
	}

	let init_st fna r = { fna; reg = r; lock = None; kreg = Regions.empty; }

	let select _fla fd fsch fna =
		let feffects = AFun.sum fna in
		let unlocked = E.(regions(filter is_unlocks feffects)) in
		L.of_enum (Enum.map (init_st fna) (Regions.enum unlocked))

	let trace st ef =
		let ef_rs = E.(regions (filter (not % is_reads) ef)) in
		Regions.(mem st.reg ef_rs)

        let may_lock r ef :bool = E.(mem (locks r) ef)
                
	let may_unlock r ef :bool =	E.(mem (unlocks r) ef)

	let not_locks r ef :bool = not (may_lock r ef)

	let may_write r ef :bool = E.(mem (writes r) ef)

	let not_writes r ef :bool = not (may_write r ef)

	(* THINK: ignore_writes can be handled by Flow2Checker. *)
	let not_writes_any rs ef :bool =
		if Opts.ignore_writes()
		then true
		else Regions.for_all (fun r -> not_writes r ef) rs

	let unlocks r ef :bool = E.(mem_must (unlocks r) ef)

	let unlocks_and_not_locks r ef = unlocks r ef && not_locks r ef

	let find_lock_object = find_in_stmt CE.find_linux_lock_in_call

	let testP1 st _ = Some st

	let testQ1 st step =
		unlocks_and_not_locks st.reg step.effs =>?
			let lock1_opt = find_lock_object step in
			let krs = Option.Infix.(
				(lock1_opt >>= Lenv.kregions_of st.fna)
				|? Regions.empty
			) in
			{st with lock = lock1_opt; kreg = krs}

	(* NOTE [Ignore writes]
	 *
	 * This is useful to conservatively ignore collections of locks, e.g. in
	 *
	 *     for (i=0; i<N; i++) spin_lock(lock_arr[i]);
	 *
	 * the `i' is written at each iteration, and that will prevent us from
	 * reporting a double lock here.
	 *)
	let testP2 st step =
(* 		if not (not_writes_any st.kreg step.effs)
		then Log.error "Some monitored region (%s) is being written at %s: %s & %s" (Regions.to_string st.kreg) (Utils.Location.to_string step.sloc) (string_of_step step) Effects.(to_string (principal step.effs)); *)
		(not_locks st.reg step.effs && not_writes_any st.kreg step.effs) =>? st

	(* THINK: match_lock_exp can be handled by Flow2Checker. *)
	let testQ2_weak st step =
		if may_unlock st.reg step.effs
		then
			let lock2_opt = find_lock_object step in
			match st.lock, lock2_opt with
			| Some lo1, Some lo2
			when Opts.match_lock_exp()
			  (* If this step satisfies "Q2-strong". *)
			  && Option.is_some (testP2 st step)
			  (* Then, we compare the two lock object expressions involved
			   * and heuristically determine if, despiste aliasing information,
			   * may not denote the same lock object.
			   *)
			  && not (CE.equal_offset lo1 lo2)
			  ->
				None
			| _, _ ->
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
