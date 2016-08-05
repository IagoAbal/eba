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
open Abs

module CE = CilExtra
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
	| Some n when n >= c_MAX_TAKEN ->
		Log.warn "Cut off search at %s (-> %s %s): edge taken too many (%d) times!"
			(Utils.Location.to_string Cil.(get_stmtLoc node.skind))
			(Utils.Location.to_string Cil.(get_stmtLoc node'.skind))
			(Utils.string_of_cil Cil.d_stmt node')
			n;
		None
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

type test_kind = TWhile of bool (* enter branch *) | TOther

type step_kind =
	| Stmt of Cil.instr list
	| Test of test_kind * Cil.exp
	| Goto of Cil.label * Cil.location (* target *)
	| Ret  of Cil.exp option

type step = {
	kind : step_kind;
	effs : Effects.t;
	sloc : Cil.location;
}

type cond = Cond of test_kind * Cil.exp * Cil.location

let tk_of_option = Option.map_default (fun b -> TWhile b) TOther

let pp_step step =
	match step.kind with
	| Stmt is ->
		PP.(semi_sep (List.map (fun i -> !^ (Utils.string_of_cil Cil.d_instr i)) is))
	| Test(tk,e) ->
		let tk_doc = match tk with
		| TWhile _ -> PP.(!^ " (loop)")
		| TOther -> PP.empty
		in
		PP.(Utils.Exp.pp e + tk_doc)
	| Goto (lbl,dst) ->
		PP.(!^ "goto" ++ !^ (Utils.string_of_cil Cil.d_label lbl) ++ !^ "->" ++ Utils.Location.pp dst)
	| Ret e_opt ->
		match e_opt with
		| None -> PP.(!^ "return")
		| Some e -> PP.(!^ "return" ++ Utils.Exp.pp e)

let string_of_step = PP.to_string % pp_step

type 'a delayed = unit -> 'a

type t = Nil
       | Assume of cond * bool * t delayed
       | Seq of step * t delayed
       | If of t delayed * t delayed


let lenv_from_step fnAbs lenv step =
	match step.kind with
	| Stmt is ->
		Lenv.from_stmt fnAbs lenv (step.effs) is
	| _else___________ ->
		lenv

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
let group_by_loc fnAbs lenv instrs :step list * E.t * Lenv.t =
	assert(not(List.is_empty instrs));
	let iss = List.group_consecutive Utils.instr_same_loc instrs in
	let steps = iss |> List.map (fun is ->
		let loc = Cil.get_instrLoc (List.hd is) in
		let ef = AFun.effect_of fnAbs loc in
		{ kind = Stmt is; effs = ef; sloc = loc; }
	) in
	let effects, lenv' = List.fold_left (fun (acc,le) step ->
		E.(step.effs + acc), lenv_from_step fnAbs le step
	) (E.none,lenv) steps
	in
	steps, effects, lenv'

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

let rec generate fnAbs visited lenv if_count node :t =
	let sk = Cil.(node.skind) in
	match sk with
	| Cil.Instr [] (* label: e.g. as a result of prepareCFG *)
	| Cil.Break _
	| Cil.Continue _
	| Cil.Loop _
	| Cil.Block _ ->
		generate_if_next fnAbs visited lenv if_count node
	| Cil.Goto(stmt_ref,loc) ->
		let nxt = fun () -> generate_if_next fnAbs visited lenv if_count node in
		let target = !stmt_ref in
		let target_labels = Cil.(target.labels) in
		assert (not (List.is_empty target_labels));
		let lbl = List.hd target_labels in
		let dst_loc = Cil.get_stmtLoc Cil.(target.skind) in
		let goto = {
			kind = Goto(lbl,dst_loc);
			effs = E.none;
			sloc = loc;
		} in
		Seq(goto,nxt)
	| Cil.Instr instrs ->
		let iss, _sumef, lenv' = group_by_loc fnAbs lenv instrs in
		let next =
			(* NB: CIL doesn't insert a return if the instr is 'exit'. *)
			Option.map_default (fun (node',visited') ->
				fun () -> generate fnAbs visited' lenv' if_count node'
			) (fun () -> Nil) (next_lone visited node)
		in
		(List.fold_right (fun s nxt ->
				(* cut off if !noret is found *)
			if (E.mem_must E.noret s.effs)
			then fun () -> Seq(s,fun () -> Nil)
			else fun () -> Seq(s,nxt)
		) iss next) ()
	| Cil.Return(e_opt,loc) ->
		let ef = AFun.effect_of fnAbs loc in
		let ret = {
			kind = Ret e_opt;
			effs = ef;
			sloc = loc;
		} in
		Seq(ret,fun () -> Nil)
	| Cil.If (e,_,_,loc) ->
		let tkind = tk_of_option (CE.is_while_test node) in
		let cond = Cond(tkind,e,loc) in
		let ef = AFun.effect_of fnAbs loc in
		let take_branch dec node' =
			match take_edge visited node node' with
			| None ->
				fun () -> Nil
			| Some visited' ->
				let lenv' = Lenv.from_test fnAbs lenv dec e in
				let alt = fun () -> generate fnAbs visited' lenv' (if_count+1) node' in
				fun () -> Assume (cond,dec,alt)
		in
		(* THINK: What would be the smart strategy when c_MAX_IF_COUNT is reached? *)
		let if_else_opt, if_then = succs_of_if node in
		let e_val = Lenv.eval lenv e in
		let left (* else *) =
			match if_else_opt, e_val with
			| None, _ ->
				fun () -> Nil
			| Some _, Lenv.Dom.NonZero ->
				Log.debug "%s: skip else branch: %s evaluates to true"
					(Utils.Location.to_string loc)
					(Utils.string_of_cil Cil.d_exp e);
				fun () -> Nil
			| Some if_else, _ ->
				if if_count <= c_MAX_IF_COUNT
				then take_branch false if_else
				else begin
					Log.warn "Cut off search at %s: too many (%d) ifS!" (Utils.Location.to_string loc) if_count;
					fun () -> Nil (* no more branching ! *)
				end
		in
		let right (* then *) =
			match e_val with
			| Lenv.Dom.Zero ->
				Log.debug "%s: skip then branch: %s evaluates to false"
					(Utils.Location.to_string loc)
					(Utils.string_of_cil Cil.d_exp e);
				fun () -> Nil
			| __else_______ -> take_branch true if_then
		in
		let test = {
			kind = Test(tkind,e);
			effs = ef;
			sloc = loc;
		} in
		let alts = fun () -> If(left,right) in
		Seq(test,alts)
	| Cil.ComputedGoto _
	| Cil.Switch _
	| Cil.TryFinally _
	| Cil.TryExcept _ ->
		Error.panic_with("PathTree.generate: found computed-goto, switch, try-finally or try-except")

and generate_if_next fnAbs visited lenv if_count node =
	match next_lone visited node with
	| None                  -> Nil
	| Some (node',visited') -> generate fnAbs visited' lenv if_count node'

(* [Note !noret]
 * We cut off the exploration of a path if we find !noret,
 * i.e. if an instruction will never return. CIL expressions
 * are free of that kind of side-effects, thus we just need
 * to check it for the `Instr' case.
 *)
let paths_of (fnAbs :AFun.t) :t delayed =
	let fd = AFun.fundec fnAbs in
	let body = Cil.(fd.sbody) in
	match Cil.(body.bstmts) with
	| []     -> fun () -> Nil
	| nd0::_ -> fun () -> generate fnAbs Edges.empty Lenv.empty 0 nd0

type path = path_entry list

and path_entry =
	| PEdec of cond * bool (* value *)
	| PEstep of step * pe_step_kind
	(* TODO: Must keep info to print param->arg mapping. *)
	| PEcall of Cil.fundec * step * path

and pe_step_kind = SKmatch | SKctx

let rec pp_entry :path_entry -> PP.doc = function
	| PEdec(Cond(tk,e,l),v) ->
		let tk_doc = match tk with
		| TOther -> PP.empty
		| TWhile b when b = v -> PP.(!^ " (enter loop)")
		| TWhile b -> PP.(!^ " (skip loop)")
		in
		let e_doc = Utils.Exp.pp e in
		let v_doc = PP.(e_doc ++ !^ "->" ++ bool v) in
		let l_doc = Utils.Location.pp l in
		PP.(!^ "(?)" ++ l_doc + colon ++ v_doc + tk_doc)
	| PEstep(step,sk) ->
		let pmark = match sk with
			| SKmatch -> "(!)"
			| SKctx   -> "(~)"
		in
		let l_doc = Utils.Location.pp step.sloc in
		PP.(!^ pmark ++ l_doc + colon ++ pp_step step)
	| PEcall(fd,_step,p') ->
		let fn = Cil.(fd.svar) in
		let loc_doc = Utils.Location.pp Cil.(fn.vdecl) in
		let p'_doc = pp_path p' in
		PP.(!^ "(#) Calling" ++ !^ Cil.(fn.vname) ++ words "defined at" ++ loc_doc + colon
			+ newline + indent p'_doc)

and pp_path = function
	| [] -> PP.(!^ "trivial")
	| pe -> PP.newline_sep (List.map pp_entry pe)

let push_dec cond b (l,xs,t) = (l,PEdec(cond,b)::xs,t)

let push_ctx step (l,xs,t) = (l,PEstep(step,SKctx)::xs,t)

let record_step trace step =
	match step.kind with
	(* Do not record CIL-generated goto *)
	| Goto(Cil.Label(_,_,false),_) -> false
	| Goto(_,_)                    -> true
	| _else                        -> trace step.effs

let match_at_loc s t = L.cons (s,[PEstep(s,SKmatch)],t) L.nil

let backtrack = L.nil

type st_pred = step -> bool

(* TODO: We should return the statement or expression together with the location *)

let rec reachable ks t ~guard ~target ~trace :(step * path * t delayed) L.t =
	let open L in
	match t() with
	| Assume(cond,b,t') ->
		map (push_dec cond b) (reachable ks t' guard target trace)
	| Seq(step,t') ->
		let ms = record_if_matches ~target step t' in
		if L.is_empty ms || ks
		then ms ^@^ keep_searching ks ~guard ~target ~trace step t'
		else ms
	| If(t1,t2) ->
		let br1 = reachable ks t1 guard target trace in
		let br2 = reachable ks t2 guard target trace in
		br1 ^@^ br2
	(* TODO: Allow to specify how many levels of Loop to investigate *)
	| __otherwise__ ->
		backtrack

and record_if_matches ~target step t' =
	if target step
	then begin
		Log.debug "TARGET reached at %s: %s" (Utils.Location.to_string step.sloc) (string_of_step step);
		match_at_loc step t'
	end
	else L.nil

and keep_searching ks ~guard ~target ~trace step t' =
	if guard step
	then begin
		(* Log.debug "reachable guard at %s" (Utils.Location.to_string step.sloc); *)
		if record_step trace step
		then L.map (push_ctx step) (reachable ks t' guard target trace)
		else reachable ks t' guard target trace
	end
	else begin
		Log.debug "UNSAT guard at %s: %s" (Utils.Location.to_string step.sloc) (string_of_step step);
		L.nil
	end

(* *************** Inlining *************** *)

let inline callerAbs step =
	match step.kind with
	| Stmt([Cil.Call(_,Cil.Lval (Cil.Var fn,Cil.NoOffset),_,_)]) ->
		begin match AFun.call callerAbs fn step.sloc with
		| None ->
			Log.warn "COULD NOT INLINE call to %s" Cil.(fn.vname);
			None
		| Some fnAbs ->
			Log.info "INLINING call to %s" Cil.(fn.vname);
			Some (fnAbs, paths_of fnAbs)
		end
	| _else ->
		Log.info "COULD NOT INLINE :-(";
		None

let inline_check_loop ~bound ~filter ~guard ~target ~trace =
	let rec loop stack b caller step =
		if b = 0
		then begin
			Log.info "INLINE_CHECK exhausted :-/";
			None
		end
		else match inline caller step with
		| None             -> None
		| Some (fnAbs, pt) ->
			let fd = AFun.fundec fnAbs in
			let ps = reachable false pt ~guard ~target ~trace
			       |> filter
			in
			go stack b fnAbs step fd ps
	and go stack b func step fd ps = match L.get ps with
	| None ->
		Log.debug "INLINE_CHECK could not reach target";
		None
	| Some ((step',path',_),_) ->
		let stack' = (fd,step,path')::stack in
		(* TODO: Pick the shortest match rather than the first one. *)
		if guard step'
		then begin
			Log.info "INLINE_CHECK succeeded :-)";
			Some (step',stack')
		end
		else loop stack' (b-1) func step'
	in
	loop [] bound

let inline_check ~bound ~filter ~guard ~target ~trace ~caller step =
	let path_of_stack stack = List.fold_left (fun p2 (fd1,stp1,p1) ->
			[PEcall(fd1,stp1,p1@p2)]
		) [] stack
	in
	inline_check_loop ~bound ~filter ~guard ~target ~trace caller step
		|> Option.map (Tuple2.map2 path_of_stack)