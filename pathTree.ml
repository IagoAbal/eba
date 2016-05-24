(* Keep it simple here, just basic reachability checking, if we want more expressivity we better integrate this with Coccinelle *)

(* TODO:
 * - Rethink how we bound depth and width.
 * - Apart from limiting how many times we take goto-ish edges,
 *   we could also put another limit on loop iterations.
 * - Maybe go back to store node x effects,
 *   which could help joining paths by accumulated effects;
 *)

open Batteries

open Type

module L = LazyList

module Edges = Map.Make(
	struct
		(* Edges are identified by the stmt-id of their source and target. *)
		type t = int * int
		let compare = Pervasives.compare
	end
)

(* We count how many times an edge has been taken. We're only interested
 * in edges that may result in loops, and an approximation to this is to
 * keep track of those edges whose target is a labeled statement
 * (presumably the target of a jump statement like `goto').
 *
 * This approximation may have limitations, yet to be found. But we
 * cannot require that the source is goto|continue|break because in CIL
 * the last statement of a loop jumps to the first statement of the loop
 * body without introducing any explicit jump.
 *)
type visited = int Edges.t

(* No labeled edge can be taken more than c_MAX_TAKEN times in each path. *)
let c_MAX_TAKEN = 1 (* NB: this seems enough for taking a loop twice *)

let is_labeled node :bool = not Cil.(List.is_empty node.labels)

(* Returns [None] if the transition is disallowed. *)
let take_edge visited node node' : visited option =
	let open Cil in
	let edge = node.sid, node'.sid in
	match Edges.Exceptionless.find edge visited with
	| Some n when n >= c_MAX_TAKEN -> None
	| _else_______________________ ->
		let visited' =
			if is_labeled node'
			then Edges.modify_def 0 edge Int.succ visited
			else visited
		in
		Some visited'

let succs_of visited node : (Cil.stmt * visited) list =
	(* NB: node.succs may be empty if 'stmt' contains an exit instruction. *)
	Cil.(node.succs) |> List.filter_map (fun node' ->
		take_edge visited node node' |>
				Option.map (fun visited' -> node',visited')
	)

let next_lone visited node : (Cil.stmt * visited) option =
	match succs_of visited node with
	| []     -> None
	| [next] -> Some next
	| _other -> Error.panic()

type step = Stmt of Cil.instr list * Cil.location
          | Test of Cil.exp        * Cil.location
          | Ret  of Cil.exp option * Cil.location

type cond = Cond of Cil.exp * Cil.location

let loc_of_step = function
	| Stmt(_,loc)
	| Test(_,loc)
	| Ret (_,loc) ->
		loc

type 'a delayed = unit -> 'a

type t = Nil
       | Assume of cond * bool * t delayed
       | Seq of step * Effects.t * t delayed
       | If of t delayed * t delayed

(* Group instructions by location
 *
 * CIL instruction blocks include instructions coming from
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
		let ef = FunAbs.effect_of fnAbs loc in
		stmt, ef
	) in
	let effects = List.fold_left (fun acc (_,ef) ->
		E.(ef + acc)
	) E.none stmts
	in
	stmts, effects

(* IF-branching is limited to 2 ^ c_MAX_IF_COUNT
 *
 * Note that in pathological cases a function may have too many paths
 * to be explored. Eg. for N sequential if statements, there are 2^N
 * paths. Another source of path explosion are switch statements
 * inside a loop.
 *)
let c_MAX_IF_COUNT = 10

(**
 * NB: Empty if-then blocks such as
 *
 *     if (c) { }
 *
 * will have only one successor node. Since CIL if guards are side-effect free,
 * we just take the _then_ branch to avoid reanalyzing equivalent path sets.
 *
 * THINK: Are there other cases apart from empty if bocks?
 *)
let succs_of_if if_node =
	match Cil.(if_node.succs) with
	| []        -> Error.panic_with "PathThree.generate(If): no succesors"
	| [thn]     -> None,     thn
	| [els;thn] -> Some els, thn
	| _else____ -> Error.panic_with "PathThree.generate(If): more than two succesors"

let rec generate fnAbs visited if_count node :t =
	let sk = Cil.(node.skind) in
	match sk with
	| Cil.Instr [] (* label: e.g. as a result of prepareCFG *)
	| Cil.Goto _
	| Cil.Break _
	| Cil.Continue _
	| Cil.Loop _
	| Cil.Block _ ->
		generate_if_next fnAbs visited if_count node
	| Cil.Instr instrs ->
		let iss, ef = group_by_loc fnAbs instrs in
		let next =
			(* NB: CIL doesn't insert a return if the instr is 'exit'. *)
			Option.map_default (fun (node',visited') ->
				fun () -> generate fnAbs visited' if_count node'
			) (fun () -> Nil) (next_lone visited node)
		in
		(List.fold_right (fun (s,ef) nxt ->
				(* cut off if !noret is found *)
			if (E.mem_must E.noret ef)
			then fun () -> Seq(s,ef,fun () -> Nil)
			else fun () -> Seq(s,ef,nxt)
		) iss next) ()
	| Cil.Return(e_opt,loc) ->
		let ret = Ret(e_opt,loc) in
		let ef = FunAbs.effect_of fnAbs loc in
		Seq(ret,ef,fun () -> Nil)
	| Cil.If (e,_,_,loc) ->
		let cond = Cond(e,loc) in
		let ef = FunAbs.effect_of fnAbs loc in
		let take_branch dec node' =
			match take_edge visited node node' with
			| None ->
				fun () -> Nil
			| Some visited' ->
				let alt = fun () -> generate fnAbs visited' (if_count+1) node' in
				fun () -> Assume (cond,dec,alt)
		in
		(* THINK: What would be the smart strategy when c_MAX_IF_COUNT is reached? *)
		let if_else_opt, if_then = succs_of_if node in
		let left (* else *) =
			match if_else_opt with
			| None -> fun () -> Nil
			| Some if_else ->
				if if_count <= c_MAX_IF_COUNT
				then take_branch false if_else
				else fun () -> Nil (* no more branching ! *)
		in
		let right (* then *) = take_branch true if_then in
		let test = Test(e,loc) in
		let alts = fun () -> If(left,right) in
		Seq(test,ef,alts)
	| Cil.ComputedGoto _
	| Cil.Switch _
	| Cil.TryFinally _
	| Cil.TryExcept _ ->
		Error.panic_with("PathTree.generate: found computed-goto, switch, try-finally or try-except")

and generate_if_next fnAbs visited if_count node =
	match next_lone visited node with
	| None                  -> Nil
	| Some (node',visited') -> generate fnAbs visited' if_count node'

(* [Note !noret]
 * We cut off the exploration of a path if we find !noret,
 * i.e. if an instruction will never return. CIL expressions
 * are free of that kind of side-effects, thus we just need
 * to check it for the `Instr' case.
 *)

let paths_of (fnAbs :FunAbs.t) :t delayed =
	let fd = FunAbs.fundec fnAbs in
	let body = Cil.(fd.sbody) in
	match Cil.(body.bstmts) with
	| []     -> fun () -> Nil
	| nd0::_ -> fun () -> generate fnAbs Edges.empty 0 nd0

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

let at_loc s t = L.cons (s,[],t) L.nil

let backtrack = L.nil

type st_pred = Effects.t -> bool

(* TODO: We should return the statement or expression together with the location *)

let rec reachable ks t ~guard ~target :(step * path * t delayed) L.t =
	let open L in
	match t() with
	| Assume(cond,b,t') ->
		let dec = Dec(cond,b) in
		map (push_dec dec) (reachable ks t' guard target)
	| Seq(step,ef,t') ->
		let ms = record_if_matches ~target step ef t' in
		if L.is_empty ms || ks
		then ms ^@^ keep_searching ks ~guard ~target step ef t'
		else ms
	| If(t1,t2) ->
		let br1 = reachable ks t1 guard target in
		let br2 = reachable ks t2 guard target in
		br1 ^@^ br2
	(* TODO: Allow to specify how many levels of Loop to investigate *)
	| __otherwise__ ->
		backtrack

and record_if_matches ~target step ef t' =
	let loc = loc_of_step step in
	if target ef
	then begin
		Log.debug "reachable target at %s" (Utils.Location.to_string loc);
		at_loc step t'
	end
	else L.nil

and keep_searching ks ~guard ~target step ef t' =
	let loc = loc_of_step step in
	if guard ef
	then begin
		Log.debug "reachable guard at %s" (Utils.Location.to_string loc);
		reachable ks t' guard target
	end
	else L.nil

(* *************** Inlining *************** *)

let inline fileAbs callerAbs = function
	| Stmt([Cil.Call(_,Cil.Lval (Cil.Var fn,Cil.NoOffset),_,_)],loc)
	when FileAbs.has_fun fileAbs fn ->
		Log.info "INLINING call to %s" Cil.(fn.vname);
		let targs = FunAbs.args_of_call callerAbs loc in
		let fnAbs = FileAbs.inst_fun fileAbs fn targs in
		Some (fnAbs, paths_of fnAbs)
	| _else ->
		Log.info "INLINING was not possible :-(";
		None

let rec inline_check ~bound ~guard ~target ~file ~caller step =
	if bound = 0
	then begin
		Log.info "INLINE_CHECK exhausted :-/";
		None
	end
	else match inline file caller step with
	| None -> None
	| Some (fnAbs, pt) ->
		let ps = reachable false pt ~guard ~target in
		(* TODO: Pick the shortest match rather than the first one. *)
		Option.bind (L.peek ps) (fun (step',_,_ as p) ->
			let ef = FunAbs.effect_of fnAbs (loc_of_step step') in
			if guard ef
			then begin
				Log.info "INLINE_CHECK succeeded :-)";
				Some p
			end
			else inline_check ~bound:(bound-1) ~guard ~target ~file ~caller:fnAbs step'
		)
