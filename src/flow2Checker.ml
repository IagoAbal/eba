(* A checker based on the CTL query: p1 EU (q1 && X(p2 EU q2))
 * or, in English, "q1 after repeatedly p1; and q2 after repeatedly p2"
 *)

open Batteries
open Dolog

module Opts = Opts.Get

open Type
open Abs
open PathTree

module L = LazyList

(* TODO: A test should be generalized to
 *     st -> stmt -> Effects.t -> st option
 * but we need to support this in PathThree.reachable first.
 *)

module type Spec = sig
	(** A name to identify the checker *)
	val name : string

	(** Checker's internal state, eg. memory regions to track. *)
	type st
	(** Selects initial contexts *)
	val select : AFile.t -> Cil.fundec -> shape scheme -> AFun.t -> st L.t
	(** Flags steps of interest for triaging. *)
	val trace : st -> Effects.t -> bool
	(** Tests *)
	val testP1 : st -> step -> st option
	val testQ1 : st -> step -> st option
	val testP2 : st -> step -> st option
	(** Q2 = P2 /\ Q2-weak *)
	val testQ2_weak : st -> step -> st option

	(** Bug data *)
	type bug
	val bug_of_st : st -> bug
	val doc_of_report : fn:Cil.varinfo -> bug -> loc1:Cil.location -> loc2:Cil.location -> trace:path -> PP.doc
end

module type S = sig
	val in_func : AFile.t -> Cil.fundec -> string L.t
end

module Make (A :Spec) : S = struct

	type report = {
		fn    : Cil.varinfo;
		bug   : A.bug;
		loc1  : Cil.location;
		loc2  : Cil.location;
		trace : path;
	}

	let string_of_report {fn; bug; loc1; loc2; trace} = PP.to_string(
		A.doc_of_report fn bug loc1 loc2 trace
	)

	let same_loc (_,s1,_,_) (_,s2,_,_) = Cil.compareLoc s1.sloc s2.sloc = 0

	let cmp_match (_,s1,p1,_) (_,s2,p2,_) :int =
		let l1 = s1.sloc in
		let l2 = s2.sloc in
		let lc = Cil.compareLoc l1 l2 in
		(* Order reversed so that L.unique_eq will take the simplest match *)
		if lc = 0
		then
			let pc = Int.compare (List.length p1) (List.length p2) in
			if pc = 0
			then -(compare p1 p2)
			else -pc
		else -lc

	(* Remove redundant traces keeping the shortest one (wrt [cmp_match]). *)
	let nodup =
		(* If we use List.t instead of LazyList.t we can achieve the same result.
		 * by composing group and map.
		 * NB: L.unique_eq is under specified, so it can change in future versions.
		 *)
		L.(unique_eq ~eq:same_loc % (sort ~cmp:cmp_match))

	(**
	 * Filter false positives due to effect ordering, ie. those Q2_weak matches
	 * that do not satisfy the guard P2. For instance, consider:
	 *
	 *     g_1(lock l) { acquire(l); release(l); }
	 *     g_2(lock l) { release(l); acquire(l); }
	 *     f_i(lock l) { acquire(l); g_i(l); release(l); }
	 *
	 * for i=1,2. Both g_1 and g_2 have exactly the same effect signature, so
	 * from f_i's point of view, they are indistinguishable. However, f_1 leads
	 * to a deadlock while f_2 is safe.
	 *
	 * If we use a strong Q2 = acquire /\ not release, we find no bug; but if we
	 * use a weaker Q2 = acquire, we incorrectly report a bug on f_2's call to
	 * g_2. The solution is to use a weaker Q2, but inline calls to g_i and find
	 * out whether the double lock acquisition is possible.
	 *)
	let filter_fp_notP2 fnAbs = L.filter_map (fun ((st,s2,p2,pt2) as t2) ->
		if Opts.inline_limit() >= 0 && Option.is_none (A.testP2 st s2)
		then begin
			Log.debug "filter_fp_notfP: inlining ...";
			let confirmed = inline_check ~bound:(Opts.inline_limit()) ~filter:nodup
				~guard:A.testP2 ~target:A.testQ2_weak
				~trace:A.trace
				~caller:fnAbs
				st
				s2
			in
			Option.bind confirmed (fun (s2',p2') ->
				(* THINK: We do NOT update the state after `inline_check',
				 * because the state should be local to the function where the
				 * bug is reported. But we DO update the program `step' to blame.
				 *)
				Some (st,s2',p2@p2',pt2)
			)
		end
		else (Log.debug "filter_fp_notfP: NOT inlining :-("; Some t2)
		)

	let search fnAbs fd pt st =
		(* p1 EU q1
		 * Even if we find a match, we keep searching for more if P1 holds.
		 * We want to find all Q1's, otherwise eg if a lock is manipulated
		 * correctly once, any subsequent manipulations of the same lock would
		 * be ignored:
		 *
		 *     lock       <--- Q1
		 *     unlock
		 *     lock       <--- ignored
		 *     lock
		 *)
		Log.info ">>> Searching for first target ...";
		let ps1 = reachable true pt
			~guard:A.testP1
			~target:A.testQ1
			~trace:A.trace
			st
		in
		(* ... => X(p2 EU q2)
		 * - Once a complete match Q1-->Q2 is found it usually makes little
		 *   sense to keep searching for more.
		 * - If we reach the same location in different ways, we just keep the
		 *   we just keep the shortest (first) one wrt [cmp_match].
		 *)
		let rss = nodup ps1 |> L.map (fun (st1,s1,p1,pt') ->
			Log.info ">>> Searching for second target starting with %s ..." (string_of_step s1);
			let ps2 = reachable false pt'
				~guard:A.testP2
				~target:A.testQ2_weak
				~trace:A.trace
				st1
			in
			(* THINK: ps2_nodup just in strict mode? *)
			let ps3 = ps2 |> nodup |> filter_fp_notP2 fnAbs in
			ps3 |> L.map (fun (st2,s2,p2,_) ->
				{ fn   = Cil.(fd.svar)
				; bug  = A.bug_of_st st2
				; loc1 = s1.sloc
				; loc2 = s2.sloc
				; trace= p1@p2
				}
			)
		)
		in
		(* TODO: define my own L.concat_map ? *)
		L.concat rss

	let in_func fileAbs fd =
		let fn = Cil.(fd.svar) in
		let fsch, fnAbs = Option.get(AFile.find_fun fileAbs fn) in
		let seeds = A.select fileAbs fd fsch fnAbs in
		let pt = paths_of fnAbs in
		let bugs = seeds |> L.map (search fnAbs fd pt) |> L.concat in
		bugs |> L.map string_of_report

end
