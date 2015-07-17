(* Keep it simple here, if we want more expressivity we better integrate this with Coccinelle *)

open Batteries

open Type

module L = LazyList

module Edges = Set.Make(
	struct
		type t = int * int
		let compare = Pervasives.compare
	end
)

type visited = Edges.t

let succs_of visited node : (Cil.stmt * visited) list =
	let open Cil in
	assert(not(List.is_empty node.succs));
	node.succs |> List.filter_map (fun node' ->
		let edge = node.sid, node'.sid in
		if Edges.mem edge visited
		then None
		else
			let visited' = Edges.add edge visited in
			Some (node', visited')
	)

let next_lone visited node : (Cil.stmt * visited) option =
	match succs_of visited node with
	| []     -> None
	| [next] -> Some next
	| _other -> Error.panic()

let next_one visited node =
	let open Option in
	let lnext = next_lone visited node in
	assert (is_some lnext);
	get lnext

type step = Stmt of Cil.instr list * Cil.location
          | Test of Cil.exp        * Cil.location
          | Ret  of Cil.exp option * Cil.location

type cond = Cond of Cil.exp * Cil.location

let loc_of_step = function
	| Stmt(_,loc)
	| Test(_,loc)
	| Ret (_,loc) ->
		loc

type t = Nil
       | Assume of cond * bool * t Lazy.t
       | Seq of step * Effects.t * t Lazy.t
       | If of t Lazy.t * t Lazy.t

(* Group instructions by location
 *
 * CIL instruction blocks include instructions comming from
 * different statements in the program. We are interested in
 * group these instructions by location. On the way, we also
 * compute the effects for each statement, and the sum of
 * effects.
 *
 * TODO: split this function into two, one groups the
 * instructions (which can go into Utils) and the other
 * takes the effects.
 *)
let group_by_loc fnAbs instrs :(step * E.t) list * E.t =
	assert(not(List.is_empty instrs));
	let iss = List.group_consecutive Utils.instr_same_loc instrs in
	let stmts = iss |> List.map (fun is ->
		let loc = Cil.get_instrLoc (List.hd is) in
		let stmt = Stmt(is,loc) in
		let ef = E.principal (FunAbs.effect_of fnAbs loc) in
		stmt, ef
	) in
	let effects = List.fold_left (fun acc (_,ef) ->
		E.(ef + acc)
	) E.none stmts
	in
	stmts, effects

let rec generate fnAbs visited node :t =
	let sk = Cil.(node.skind) in
	match sk with
	| Cil.Instr [] (* label: e.g. as a result of prepareCFG *)
	| Cil.Goto _
	| Cil.Break _
	| Cil.Continue _
	| Cil.Loop _
	| Cil.Block _ ->
		generate_if_next fnAbs visited node
	| Cil.Instr instrs ->
		let node', visited' = next_one visited node in
		let iss, ef = group_by_loc fnAbs instrs in
		let next = lazy(generate fnAbs visited' node') in
		Lazy.force(List.fold_right (fun (s,ef) nxt ->
				(* cut off if !noret is found *)
			if (E.mem_must E.noret ef)
			then lazy(Seq(s,ef,lazy Nil))
			else lazy(Seq(s,ef,nxt))
		) iss next)
	| Cil.Return(e_opt,loc) ->
		let ret = Ret(e_opt,loc) in
		let ef = FunAbs.effect_of fnAbs loc in
		Seq(ret,ef,lazy Nil)
	| Cil.If (e,_,_,loc) ->
		let cond = Cond(e,loc) in
		let ef = FunAbs.effect_of fnAbs loc in
		let succs = Utils.match_pair (succs_of visited node) in
		let (alt1,alt2) = succs |> Tuple2.mapn (fun (node',visited') ->
			lazy(generate fnAbs visited' node')
		) in
		let test = Test(e,loc) in
		let alts =
			let left  = lazy (Assume(cond,false,alt1))  in
			let right = lazy (Assume(cond,true,alt2)) in
			lazy(If(left,right))
		in
		Seq(test,ef,alts)
	| Cil.ComputedGoto _
	| Cil.Switch _
	| Cil.TryFinally _
	| Cil.TryExcept _ ->
		Error.not_implemented()

and generate_if_next fnAbs visited node =
	match next_lone visited node with
	| None                  -> Nil
	| Some (node',visited') -> generate fnAbs visited' node'

(* [Note !noret]
 * We cut off the exploration of a path if we find !noret,
 * i.e. if an instruction will never return. CIL expressions
 * are free of that kind of side-effects, thus we just need
 * to check it for the `Instr' case.
 *)

let paths_of (fnAbs :FunAbs.t) (fd :Cil.fundec) :t Lazy.t =
	Cil.prepareCFG fd;
	Cil.computeCFGInfo fd false;
	let body = Cil.(fd.sbody) in
	match Cil.(body.bstmts) with
	| []     -> Lazy.from_val Nil
	| nd0::_ ->	lazy(generate fnAbs Edges.empty nd0)

type path_dec = Dec of cond * bool

let pp_dec (Dec(Cond(_,l),v)) :PP.doc =
	let v_doc = PP.bool v in
	let l_doc = Utils.Location.pp l in
	PP.(v_doc ++ !^ "at" ++ l_doc)

type path = path_dec list

let pp_path = function
	| [] -> PP.(!^ "trivial")
	| p  -> PP.newline_sep (List.map pp_dec p)

let push_dec x (l,xs,t) = (l,x::xs,t)

let at_loc l t = L.cons (l,[],t) L.nil

let backtrack = L.nil

type st_pred = Effects.t -> bool

(* TODO: We should return the statement or expression together with the location *)

let rec reachable t ~guard ~target :(Cil.location * path * t Lazy.t) L.t =
	let open L in
	match Lazy.force t with
	| Assume(cond,b,t') ->
		let dec = Dec(cond,b) in
		map (push_dec dec) (reachable t' guard target)
	| Seq(step,ef,t') ->
	   record_if_matches ~target step ef t'
		^@^ keep_searching ~guard ~target step ef t'
	| If(t1,t2) ->
		let br1 = reachable t1 guard target in
		let br2 = reachable t2 guard target in
		br1 ^@^ br2
	(* TODO: Allow to specify how many levels of Loop to investigate *)
	| __otherwise__ ->
		backtrack

and record_if_matches ~target step ef t' =
	let loc = loc_of_step step in
	if target ef
	then begin
		Log.debug "reachable target at %s" (Utils.Location.to_string loc);
		at_loc loc t'
	end
	else L.nil

and keep_searching ~guard ~target step ef t' =
	let loc = loc_of_step step in
	if guard ef
	then begin
		Log.debug "reachable guard at %s" (Utils.Location.to_string loc);
		reachable t' guard target
	end
	else L.nil
