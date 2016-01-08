(* A checker based on the CTL query: p1 EU (q1 && X(p2 EU q2))
 * or, in English, "q1 after repeatedly p1; and q2 after repeatedly p2"
 *)

open Batteries

open Type
open PathTree

module L = LazyList

(* TODO: A test should be generalized to
 *     st -> stmt -> Effects.t -> st option
 * but we need to support this in PathThree.reachable first.
 *)

module type Spec = sig
	(** A name to identify the checker *)
	val name : string

	(** Checker's internal state *)
	type st
	(** Selects initial states *)
	val select : FileAbs.t -> Cil.fundec -> shape scheme -> FunAbs.t -> st L.t
	(** Tests *)
	val testP1 : st -> Effects.t -> bool
	val testQ1 : st -> Effects.t -> bool
	val testP2 : st -> Effects.t -> bool
	val testQ2 : st -> Effects.t -> bool

	(** Bug data *)
	type bug = st
	val bug_of_st : st -> bug
	val doc_of_report : fn:Cil.varinfo -> bug -> loc1:Cil.location -> loc2:Cil.location -> trace:path -> PP.doc
end

module type S = sig
	val in_func : FileAbs.t -> Cil.fundec -> string L.t
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

	let same_loc (l1,_,_) (l2,_,_) = l1 = l2

	let cmp_match (l1,p1,_) (l2,p2,_) :int =
		(* Order reversed so that L.unique_eq will take the simplest match *)
		let lc = compare l1 l2 in
		if lc = 0
		then
			let pc = compare (List.length p1) (List.length p2) in
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

	let search fd pt st =
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
		let ps1 = reachable true pt
			~guard:(A.testP1 st)
			~target:(A.testQ1 st)
		in
		(* ... => X(p2 EU q2)
		 * - Once a complete match Q1-->Q2 is found it usually makes little
		 *   sense to keep searching for more.
		 * - If we reach the same location in different ways, we just keep the
		 *   we just keep the shortest (first) one wrt [cmp_match].
		 *)
		let rss = nodup ps1 |> L.map (fun (l1,p1,pt') ->
			let ps2 = reachable false pt'
				~guard:(A.testP2 st)
				~target:(A.testQ2 st)
			in
			(* THINK: ps2_nodup just in strict mode? *)
			nodup ps2 |> L.map (fun (l2,p2,_) ->
				{ fn   = Cil.(fd.svar)
				; bug  = A.bug_of_st st
				; loc1 = l1
				; loc2 = l2
				; trace= p1@p2
				}
			)
		)
		in
		(* TODO: define my own L.concat_map ? *)
		L.concat rss

	let in_func fileAbs fd =
		let fn = Cil.(fd.svar) in
		let fsch, fnAbs = FileAbs.find_fun fileAbs fn in
		let seeds = A.select fileAbs fd fsch fnAbs in
		let pt = paths_of fnAbs fd in
		let bugs = seeds |> L.map (search fd pt) |> L.concat in
		bugs |> L.map string_of_report

end
