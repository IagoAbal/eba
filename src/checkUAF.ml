
open Batteries

open Type
open Abs
open PathTree

open Utils.Option

module CE = CilExtra

module L = LazyList

module Spec = struct
	let name = "Flow-2 use-after-free checker"

	type st = {
		fna  : AFun.t;
		reg  : Region.t;
		obj  : Cil.exp option;
		freg : Regions.t;
		kreg : Regions.t;
	}

	let init_st fna r = { fna; reg = r; obj = None; freg = Regions.empty; kreg = Regions.empty; }

	let select _fla fd fsch fna =
		let feffects = AFun.sum fna in
		let freed = E.(regions(filter is_frees feffects)) in
		L.of_enum (Enum.map (init_st fna) (Regions.enum freed))

	let trace st ef =
		let ef_rs = E.(regions ef) in
		Regions.(mem st.reg ef_rs)

	(* let may_unlock r ef :bool =	E.(mem (unlocks r) ef) *)

	(* let not_unlocks r ef :bool = not (may_unlock r ef) *)

	let may_write r ef :bool = E.(mem (writes r) ef)

	let not_writes r ef :bool = not (may_write r ef)

	(* THINK: ignore_writes can be handled by Flow2Checker. *)
	let not_writes_any rs ef :bool =
		(* if Opts.ignore_writes()
		then true
		else  *)
		Regions.for_all (fun r -> not_writes r ef) rs

	let frees r ef :bool = E.(mem_must (frees r) ef)

	let uses r ef :bool = E.(mem (frees r) ef)

	let may_use r ef :bool = E.(mem (frees r) ef)

	let may_use_any rs ef :bool = Regions.exists (fun r -> may_use r ef) rs

	(* let frees_and_not_writes_any r rs ef = frees r ef && not_writes_any rs ef *)

	let find_freed_object = find_in_stmt CE.(find_arg_in_call pick_first_arg)

	let testP1 st _ = Some st

	(* NOTE: If we inline until finding the kfree then we don't need this! *)
	let find_free_obj fna tr arg =
		let arg_z = Lenv.shape_of fna arg in
		let open Type in
		let open Shape in
		let rec find_it n e = function
		| _ when n = 0 -> None
		| Var _ | Bot | Fun _ -> None
		| Ref _ -> Error.panic_with("find_free_obj: ref shape")
		| Ptr(Ref(r,z)) ->
			if Region.equal tr r
			then Some e
			else
				let e1 = e in (* THINK: Cil.(Lval (Mem e,NoOffset)) in *)
				find_it n e1 z
		| Struct s -> find_it_in_members n e (list_fields s)
		| Ptr _ -> Error.panic_with("find_free_object: invalid Ptr shape")
		and find_it_in_members n e = function (* This is some kind of iterator *)
		| [] -> None
		| m::ms when Regions.mem tr (regions_in m.fshape) ->
			let e1 = Cil.(Lval(Mem e,Field(m.finfo,NoOffset))) in
			begin match find_it_in_member n e1 m with
			| Some e2 -> Some e2
			| None    -> find_it_in_members n e ms
			end
		| _m::ms -> find_it_in_members n e ms
		and find_it_in_member n e m =
			match m.fshape with
			| Ref(r,z) ->
				(* Log.error "find_free_obj (tr: %s): %s : ref %s %s" (Region.to_string tr) (Utils.string_of_cil Cil.d_exp e) (Region.to_string r) (Shape.to_string z); *)
				if Region.equal tr r
				then Some e
				else find_it (n-1) e z
			| _else__ -> Error.panic_with("find_free_obj: invalid member shape")
		in
		Option.Infix.(arg_z >>= find_it 2 arg)


	let testQ1 st step =
		let open Option.Infix in
		if frees st.reg step.effs
		then begin
			let obj_opt0 = find_freed_object step in
			(* Option.may (fun o -> Log.error "o0: %s" (Utils.string_of_cil Cil.d_exp o)) obj_opt0; *)
			let obj_opt = Option.Infix.(obj_opt0 >>= find_free_obj st.fna st.reg) in
			(* Option.may (fun o -> Log.error "o1: %s" (Utils.string_of_cil Cil.d_exp o)) obj_opt; *)
			let frs = Option.Infix.(
				(obj_opt >>= Lenv.shape_of st.fna >>= fun z -> Some (Shape.sregions_of z))
				|? Regions.empty
			) in
			let krs = Option.Infix.(
				(obj_opt >>= Lenv.kregions_of st.fna)
				|? Regions.empty
			) in
			(* Log.error "testQ1: reg = %s ; freg = %s" (Region.to_string st.reg) (Regions.to_string frs); *)
			if not_writes_any krs step.effs
			then Some {st with obj = obj_opt; freg = frs; kreg = krs}
			else None
		end
		else None

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
		not_writes_any st.kreg step.effs =>? st

	(* THINK: match_lock_exp can be handled by Flow2Checker. *)
	let testQ2_weak st step =
		if may_use_any st.freg step.effs
		then
			let obj2_opt = find_freed_object step in
			match st.obj, obj2_opt with
			| Some lo1, Some lo2
			when Opts.match_lock_exp()
			  (* If this step satisfies "Q2-strong". *)
			  && Option.is_some (testP2 st step)
			  (* Then, we compare the two object expressions involved
			   * and heuristically determine if, despiste aliasing information,
			   * may not denote the same object.
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
		words "Use after free" ++ parens(Region.pp r) + newline
		++ words "freed at" ++ (Utils.Location.pp loc1) + newline
		++ words "used at" ++ (Utils.Location.pp loc2) + newline
		+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
		++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
		+ PathTree.pp_path trace

end

module Checker = Flow2Checker.Make(Spec)

include Checker
