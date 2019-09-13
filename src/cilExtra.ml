open Batteries
open Cil

(* TODO: We can also negate Le, Ge, Lt and Gt *)
let negateExp = function
| Const(CInt64(c,k,str)) when c = Int64.zero ->
	Some(Const(CInt64(Int64.one,k,str)))
| Const(CInt64(c,k,str)) when c <> Int64.zero ->
	Some(Const(CInt64(Int64.zero,k,str)))
| BinOp(Eq,a1,b1,ty1) ->
	Some(BinOp(Ne,a1,b1,ty1))
| BinOp(Ne,a1,b1,ty1) ->
	Some(BinOp(Eq,a1,b1,ty1))
| _else______________ ->
	None

(** Like Cil.stripCast but it also recurses into operations. *)
let rec stripCastsOp = function
| CastE(_, e)        -> stripCastsOp e
| UnOp(op,e,ty)      -> UnOp(op,stripCastsOp e,ty)
| BinOp(op,e1,e2,ty) -> BinOp(op,stripCastsOp e1,stripCastsOp e2,ty)
| e                  -> e

(* Function call arguments *)

let args_of_call : instr -> exp list option = function
| Call(_,_,args,_) -> Some args
| _else___________ -> None

let pick_arg_that pred instr : exp option =
	let open Option.Infix in
	args_of_call instr >>= List.Exceptionless.find pred

let pick_first_arg instr : exp option =
	let open Option.Infix in
	args_of_call instr >>= List.Exceptionless.hd

let arg_is_linux_lock e :bool =
	match unrollTypeDeep (typeOf e) with
	| TPtr (TComp(ci,_),_)
	(* THINK: more? *)
	when ci.cname = "spinlock" || ci.cname = "mutex" -> true
	| _ ->
		false

let find_arg_in_call pick instrs : exp option =
	List.Exceptionless.find_map pick instrs
		|> Option.map Cil.stripCasts

let find_linux_lock_in_call : instr list -> exp option =
	let open Utils.Option in
	find_arg_in_call (pick_arg_that arg_is_linux_lock <|> pick_first_arg)

(* THINK: Some of the functions below would not be needed if our front-end, CIL,
 * would keep more information about loops; and if EBA would not rely on
 * `Cil.computeCFGInfo', which simplifies the CFG removing `switch'.
 *)

let compareType ty1 ty2 = Pervasives.compare (typeSig ty1) (typeSig ty2)

let compareTypeAnd ty1 ty2 delayed_cmp =
	let ty_cmp = compareType ty1 ty2 in
	if ty_cmp <> 0
	then ty_cmp
	else delayed_cmp()

(* A (hopefully) sensible way of comparing CIL expressions. *)
let rec compareExp exp1 exp2 =
	match exp1, exp2 with
	| Const c1, Const c2         -> Pervasives.compare c1 c2
	| Lval lv1, Lval lv2
	| AddrOf lv1, AddrOf lv2
	| StartOf lv1, StartOf lv2   -> compareLval lv1 lv2
	| SizeOf ty1, SizeOf ty2
	| AlignOf ty1, AlignOf ty2   -> compareType ty1 ty2
	| SizeOfE e1, SizeOfE e2
	| AlignOfE e1, AlignOfE e2   -> compareExp e1 e2
	| SizeOfStr s1, SizeOfStr s2 -> String.compare s1 s2
	| UnOp(o1,e1,ty1), UnOp(o2,e2,ty2) ->
		let o_cmp = Pervasives.compare o1 o2 in
		if o_cmp <> 0
		then o_cmp
		else compareTypeAnd ty1 ty2 (fun () -> compareExp e1 e2)
	| BinOp(o1,a1,b1,ty1), BinOp(o2,a2,b2,ty2) ->
		let o_cmp = Pervasives.compare o1 o2 in
		if o_cmp <> 0
		then o_cmp
		else compareTypeAnd ty1 ty2 (fun () -> compareExp2 a1 a2 b1 b2)
	| Question(a1,b1,c1,ty1), Question(a2,b2,c2,ty2) ->
		compareTypeAnd ty1 ty2 (fun () -> compareExp3 a1 a2 b1 b2 c1 c2)
	| CastE(ty1,e1), CastE(ty2,e2) ->
		compareTypeAnd ty1 ty2 (fun () -> compareExp e1 e2)
	| AddrOfLabel stmt_ref1, AddrOfLabel stmt_ref2 ->
		Int.compare !stmt_ref1.sid !stmt_ref2.sid
	(* NB: Should be different constructors, so we let the default `compare'
	 * determine the order betwen them.
	 *)
	| e1, e2 -> Pervasives.compare e1 e2

and compareExp2 a1 a2 b1 b2 =
	let a_cmp = compareExp a1 a2 in
	if a_cmp <> 0
	then a_cmp
	else compareExp b1 b2

and compareExp3 a1 a2 b1 b2 c1 c2 =
	let a_cmp = compareExp a1 a2 in
	if a_cmp <> 0
	then a_cmp
	else compareExp2 b1 b2 c1 c2

(* A (hopefully) sensible way of comparing CIL lvalues. *)
and compareLval (lhost1,offset1) (lhost2,offset2) =
	let lhost_cmp = compareLhost lhost1 lhost2 in
	if lhost_cmp <> 0
	then lhost_cmp
	else compareOffset offset1 offset2

and compareLhost lhost1 lhost2 =
	match lhost1, lhost2 with
	(* NB: We use a relaxed criterion that compares variable names rather than
	 * `vid's, so that two local variables from two different functions can be
	 * considered equal, if their names are equal.
	 *
	 * Eg. If `f' locks on `obj->lock' and calls `g' which also locks on `obj->lock',
	 * then the two expressions are consired equal, even though each one of two `obj'
	 * variables is different.
	 *)
	| Var x , Var y  -> String.compare x.vname y.vname
	| Mem e1, Mem e2 -> compareExp e1 e2
	| Var _ , Mem _  -> -1
	| Mem _ , Var _  -> 1

and compareOffset offset1 offset2 =
	match offset1, offset2 with
	| NoOffset, NoOffset -> 0
	| NoOffset, _ -> -1
	| Field(fi1,of1), Field(fi2,of2) ->
		let s_cmp = Int.compare fi1.fcomp.ckey fi2.fcomp.ckey in
		if s_cmp <> 0
		then s_cmp
		else let fi_cmp = String.compare fi1.fname fi2.fname in
		if fi_cmp <> 0
		then fi_cmp
		else compareOffset of1 of2
	| Field _, NoOffset -> 1
	| Field _, Index _  -> -1
	| Index(e1,of1), Index(e2,of2) ->
		let e_cmp = compareExp e1 e2 in
		if e_cmp <> 0
		then e_cmp
		else compareOffset of1 of2
	| Index _, NoOffset -> 1
	| Index _, Field _  -> 1

(* Compares the offsets of two expressions for equality.
 *
 * NB: We can use this as an heuristic to discard aliasing relationships when
 * the offsets do not match. Unfortunately, our alias analysis doesn't deal well
 * with dynamic data structures. Thus we will identify, for instance, `obj->lock'
 * and `obj->parent->lock' as the same lock object.
 *)
let equal_offset exp1 exp2 =
	match exp1, exp2 with
	(* Expected case is for instance x.lock or x->lock. *)
	| Lval(_,o1), Lval(_,o2)
	| AddrOf(Mem _,o1), AddrOf(Mem _,o2) -> compareOffset o1 o2 = 0
	(* Function calls that won't be inlined because P2 holds may just pass the
	 * whole object. eg. &(hi->lock) vs hi, so in this case we compare the lhost.
	 *)
	| AddrOf(Mem e1,_), e2   -> compareExp e1 e2 = 0
	(* Otherwise, we require both expressions to be syntactically equal. *)
	| _                      -> compareExp exp1 exp2 = 0

(** Has `node' a CIL-generated label starting with `prefix' ? *)
let is_labeled_with prefix node :bool =
	node.labels |> List.exists (function
	| Label(n,_,false) -> String.starts_with n prefix
	| _ -> false
	)

let is_labeled_while_continue = is_labeled_with "while_continue"

let is_labeled_while_break = is_labeled_with "while_break"

(** Is `node' a goto to a node satisfying `pred' ? *)
let is_goto_with pred node =
	match node.skind with
	| Goto (stmt_ref,_) ->
		pred !stmt_ref
	| _else________________ ->
		false

let is_while_break = is_goto_with is_labeled_while_break

(** Is `node' the entry test of a loop?
 * If so, what is the valuation required to take the loop?
 *
 * is_while_test (if (e) S) = Some true  ---> while (e)  { ... }
 * is_while_test (if (e) S) = Some false ---> while (!e) { ... }
 *)
let is_while_test node : bool option =
	(* Log.info "%d -> %s at %s\n" node.sid (Utils.string_of_cil Cil.d_stmt node) PP.(to_string (Utils.Location.pp (get_stmtLoc node.skind)));
	node.preds |> List.iter (fun node' ->
		Log.info "%d Pred %s\n" node.sid (Utils.string_of_cil Cil.d_stmt node');
		node'.preds |> List.iter (fun node'' ->
			Log.info "%d Pred' %s\n" node.sid (Utils.string_of_cil Cil.d_stmt node'');
		);
	); *)
	match node.skind with
	| If _ ->
		let there_is_while_continue_in = List.exists is_labeled_while_continue in
		let is_suc_of_while_continue =
			there_is_while_continue_in node.preds
		(* or, in case the expression test were side-effectful *)
		|| (if List.is_empty node.preds
			then false
			else there_is_while_continue_in (List.hd node.preds).preds
		    )
		in
		if is_suc_of_while_continue
		then begin
			assert (List.length node.succs = 2);
			if is_while_break (List.nth node.succs 0)
			then Some true  (* while (c)  ---> if (!c) break; *)
			else if is_while_break (List.nth node.succs 1)
			then Some false (* while (!c) ---> if (c) break;  *)
			else None
		end
		else None
	| _else -> Error.panic_with("CilExtra.is_while_test: not an `if' statement")
