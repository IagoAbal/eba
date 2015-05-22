(* Keep it simple here, if we want more expressivity we better integrate this with Coccinelle *)

open Batteries

open Type

module L = LazyList

type state = {
	node    : Cil.stmt;
	effects : Effects.t;
}

module States = Set.Make(
	struct
		type t = state
		let compare s1 s2 =
			let node_cmp = Cil.(compare s1.node.sid s2.node.sid) in
			if node_cmp = 0
			then Effects.compare s1.effects s2.effects
			else node_cmp
	end
)

type visited = States.t

type rexp = Rexp of Cil.exp option * Cil.location

type stmt = Stmt of Cil.instr list * Cil.location

type cond = Cond of Cil.exp * Cil.location

(* THINK: Instead of having Nil, paths_of could return t Lazy.t option *)

type t = Nil
       | Return of rexp * Effects.t
       | Seq of stmt * Effects.t * t Lazy.t
	   (* TODO: We should split the If node into two, in case we want
	    * to resume a reachability query from an `if' without examining
	    * the condition again. Fortunately, expressions have limited
	    * effects in CIL, mostly read-only.
	    *)
       | If of cond * Effects.t * t Lazy.t * t Lazy.t

let if_not_visited visited st f =
	if States.mem st visited
	then Nil
	else f()

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
let group_by_loc fnAbs instrs :(stmt * E.t) list * E.t =
	assert(not(List.is_empty instrs));
	let iss = List.group_consecutive Utils.instr_same_loc instrs in
	let stmts = iss |> List.map (fun is ->
		let loc = Cil.get_instrLoc (List.hd is) in
		let stmt = Stmt(is,loc) in
		let ef = FunAbs.effect_of fnAbs loc in
		stmt, ef
	) in
	let effects = List.fold_left (fun acc (_,ef) ->
		E.(ef + acc)
	) E.none stmts
	in
	stmts, effects

let rec generate fnAbs visited st :t =
	if_not_visited visited st (fun () ->
		let visited' = States.add st visited in
		let {node; effects} = st in
		let sk = Cil.(node.skind) in
		match sk with
		| Cil.Instr [] (* label: e.g. as a result of prepareCFG *)
		| Cil.Goto _
		| Cil.Break _
		| Cil.Continue _
		| Cil.Loop _
		| Cil.Block _ ->
			let succs = Cil.(st.node.succs) in
			assert(not(List.is_empty succs));
			let node' = List.hd succs in
			let st' = { st with node = node' } in
			generate fnAbs visited' st'
		| Cil.Instr instrs ->
			let succs = Cil.(node.succs) in
			assert(not(List.is_empty succs));
			let node' = List.hd succs in
			let iss, ef = group_by_loc fnAbs instrs in
			let effects' = E.(ef + effects) in
			let st' = { node = node'; effects = effects' } in
			let next = lazy(generate fnAbs visited' st') in
			Lazy.force(List.fold_right (fun (s,ef) nxt ->
				lazy(Seq(s,ef,nxt))
			) iss next)
		| Cil.Return(e_opt,loc) ->
			let ef = FunAbs.effect_of fnAbs loc in
			let rexp = Rexp(e_opt,loc) in
			Return(rexp,ef)
		| Cil.If (e,_,_,loc) ->
			let cond = Cond(e,loc) in
			let ef = FunAbs.effect_of fnAbs loc in
			let effects' = E.(ef + st.effects) in
			let succs = Utils.match_pair Cil.(st.node.succs) in
			let (alt1,alt2) = succs |> Tuple2.mapn (fun node' ->
				let st' = { node = node'; effects = effects' } in
				lazy(generate fnAbs visited' st')
			) in
			If(cond,ef,alt1,alt2)
		| Cil.ComputedGoto _
		| Cil.Switch _
		| Cil.TryFinally _
		| Cil.TryExcept _ ->
			Error.not_implemented()
	)

let paths_of (fnAbs :FunAbs.t) (fd :Cil.fundec) :t Lazy.t =
	Cil.prepareCFG fd;
	Cil.computeCFGInfo fd false;
	let body = Cil.(fd.sbody) in
	match Cil.(body.bstmts) with
	| []     -> Lazy.from_val Nil
	| nd0::_ ->
		let st0 = { node = nd0; effects = E.none } in
		lazy(generate fnAbs States.empty st0)

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
	match Lazy.force t with
	| Return(Rexp(_,loc),ef) when target ef ->
		Log.info "reachable target at %s" (Utils.Location.to_string loc);
		at_loc loc (lazy Nil)
	| Seq(Stmt(_,loc),ef,t') when target ef ->
		Log.info "reachable target at %s" (Utils.Location.to_string loc);
		at_loc loc t'
	| Seq(Stmt(_,loc),ef,t') when guard ef ->
		Log.info "reachable guard at %s" (Utils.Location.to_string loc);
		reachable t' guard target
	| If(Cond(_,loc),ef,_,_) when target ef ->
		Log.info "reachable target at %s" (Utils.Location.to_string loc);
		at_loc loc t
	| If(c,ef,t1,t2) when guard ef ->
		let dec1 = Dec(c,false) in
		let dec2 = Dec(c,true) in
		let br1 = L.map (push_dec dec1) (reachable t1 guard target) in
		let br2 = L.map (push_dec dec2) (reachable t2 guard target) in
		L.append br1 br2
	(* TODO: Allow to specify how many levels of Loop to investigate *)
	| __otherwise__ ->
		backtrack
