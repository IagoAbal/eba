(* FUTURE: Common interface, but several implementations with different trade-offs. *)

open Batteries
open Dolog

open Type
open Abs

open Cil

module CE = CilExtra

(*************************************************)
(* Shape inference for (a subset of) expressions *)
(*************************************************)

(** Infers the shape and storage regions of an expression.
 *
 * TODO: This should be simpler once we have an effect-annotated AST.
 *)
module InferShape = struct

	open Type
	open Abs

	open Option.Infix

	let rec of_lhost fna = function
	| Var x -> AFun.find_var fna x >>= Shape.(function
			| Ref(r,z) -> Some (Ref(r,z), Regions.singleton r)
			| _else___ -> None
			)
	| Mem e -> of_exp fna e >>= Shape.(function
			| Ptr z, rs -> Some (z,rs)
			| _else________, __ ->  None
			)

	(* THINK: We could perform Region.add in of_exp/Lval rather than in of_lval
	 * and of_offset. But, what to do with of_exp/AddrOf, do we include the
	 * target region of the pointer in the kregion set?
	 *)
	and of_lval fna (lhost,offset) =
		of_lhost fna lhost >>= fun res ->
			of_offset fna res offset

	and of_offset fna (z,rs) offset =
		let open Shape in
		match z, offset with
		| _any_z_________, NoOffset          ->
			Some (z,rs)
		| Ref(_,Struct cz), Field(fi,offset') ->
			let r1,z1 = get_ref (field cz fi.fname) in
			let rs'   = Regions.add r1 rs in
			of_offset fna (Ref(r1,z1),rs') offset'
		| Ref(_,Ptr(Ref(r1,z1))), Index(e,offset')  ->
			of_exp fna e >>= fun (_,rs1) ->
				let rs' = Regions.(add r1 (rs + rs1)) in
				of_offset fna (Ref(r1,z1),rs') offset'
		| _else__________, _any_offset______ ->
			None

	and of_exp fna : exp -> (shape * Regions.t) option = function
	| Const (CInt64 _) -> Some(Shape.Bot, Regions.empty)
	| Lval lv          ->
		(of_lval fna lv >>= function
		| Shape.Ref(r,z), rs -> Some(z,rs)
		| z, _else__________ -> None
		)
	| AddrOf lv        ->
		of_lval fna lv >>= fun (z,rs) ->
			Some(Shape.Ptr z,rs)
	| CastE(_,e)       -> of_exp fna e
	| BinOp(Eq,e1,e2,_)
	| BinOp(Ne,e1,e2,_)
	| BinOp(Lt,e1,e2,_)
	| BinOp(Gt,e1,e2,_)
	| BinOp(Le,e1,e2,_)
	| BinOp(Ge,e1,e2,_) ->
		of_exp fna e1 >>= fun (_,rs1) ->
		of_exp fna e2 >>= fun (_,rs2) ->
			Some (Shape.Bot,Regions.(rs1 + rs2))
	| _else___________ -> None

end

let shape_of fna = Option.map Tuple2.first % InferShape.of_exp fna

(** Compute the kill-regions of an expression:
 * The set of regions that, if updated, "kill" any fact known about an object.
 *)
let kregions_of fna = Option.map Tuple2.second % InferShape.of_exp fna

(***********************************************************)
(* Definition of the abstract state tracked by the checker *)
(***********************************************************)

module ExpMap = Map.Make(
	struct
		type t = exp
		let compare = CE.compareExp
	end
	)

module ExpSet = Set.Make(
	struct
		type t = exp
		let compare = CE.compareExp
	end
	)

module RegionMap = Map.Make(Region)

(** Abstract domain. *)
module Dom = struct

	type t = NonZero | Zero | Dunno

	let of_option = function
	| None       -> Dunno
	| Some true  -> NonZero
	| Some false -> Zero

	let to_option = function
	| Dunno   -> None
	| NonZero -> Some true
	| Zero    -> Some false

	let lnot = function
	| Dunno   -> Dunno
	| NonZero -> Zero
	| Zero    -> NonZero

	let eq x y = match x,y with
	| Zero,Zero    -> NonZero
	| Zero,NonZero -> Zero
	| NonZero,Zero -> Zero
	| __else______ -> Dunno

	let ne x y = match x,y with
	| Zero,Zero    -> Zero
	| Zero,NonZero -> NonZero
	| NonZero,Zero -> NonZero
	| __else______ -> Dunno

	let lt x y = match x,y with
	| Zero,Zero    -> Zero
	| __else______ -> Dunno

end

type t = {
	(** Abstraction: e -> != 0 (true) | == 0 (false) | don't-know *)
	facts : bool ExpMap.t;
	(** r -> e: if r is modified then forget anything you know about e *)
	kregs : ExpSet.t RegionMap.t;
}

let empty = { facts = ExpMap.empty; kregs = RegionMap.empty }

(** Record a fact `v' about some expression `e'. *)
let gen fna lenv e v =
	let open Option.Infix in
	match kregions_of fna e with
	| Some rs ->
		Log.debug "LEARNT %s -> %b (kr: %s)" (Utils.Exp.to_string e) v (Regions.to_string rs);
		{ facts = ExpMap.add e v lenv.facts
		; kregs =
			Enum.fold (fun kr r ->
				RegionMap.modify_def ExpSet.empty r (ExpSet.add e) kr
				)
				lenv.kregs
				(Regions.enum rs)
		}
	| None -> lenv

(** Kill all the facts related to a region `r' that has been updated. *)
let kill_by_region lenv r =
	match RegionMap.Exceptionless.find r lenv.kregs with
	| None   -> lenv
	| Some es ->
		{ facts =
			Enum.fold (fun fs e ->
				Log.debug "Forget that we LEARNed - %s -" (Utils.Exp.to_string e);
				ExpMap.remove e fs
				)
				lenv.facts
				(ExpSet.enum es)
		; kregs = RegionMap.remove r lenv.kregs
		}

(* Shortcut *)
let find_exp lenv e =
	match ExpMap.Exceptionless.find e lenv.facts with
	| Some v  -> Some v
	| None    ->
	(* TODO:
	 * Besides negating, we can also "reverse it", so if we cannot find
	 * a < b, then we can search for b > a instead.
	 *)
	match CE.negateExp e with
	| Some e1 -> Option.map not (ExpMap.Exceptionless.find e1 lenv.facts)
	| None    -> None

(***************************************************)
(* Evaluation of expressions under the abstraction *)
(***************************************************)

let val_of_exp lenv e = Dom.of_option (find_exp lenv e)

let try_eval_def lenv e v =
	match v with
	| Dom.Dunno -> val_of_exp lenv e
	| _else____ -> v

let rec eval_exp lenv exp : Dom.t =
	match CE.stripCastsOp exp with
	| Const(CInt64(i,_,_)) ->
		if i = Int64.zero
		then Dom.Zero
		else Dom.NonZero
	| UnOp(LNot,e,_) ->
		Dom.lnot (eval_exp lenv e)
	| BinOp(Eq,e1,e2,_) ->
		try_eval_def lenv exp (
			let v1 = eval_exp lenv e1 in
			let v2 = eval_exp lenv e2 in
			Dom.eq v1 v2
		)
	| BinOp(Ne,e1,e2,_) ->
		try_eval_def lenv exp (
			let v1 = eval_exp lenv e1 in
			let v2 = eval_exp lenv e2 in
			Dom.ne v1 v2
		)
	| BinOp(Lt,e1,e2,_)
	| BinOp(Gt,e2,e1,_) ->
		try_eval_def lenv exp (
			let v1 = eval_exp lenv e1 in
			eval_lt lenv v1 e2
		)
	| e ->
		val_of_exp lenv e

and eval_lt lenv v1 e2 =
	match v1, e2 with
	| Dom.Zero, Const(CInt64(i,_,_)) when i <= Int64.zero ->
		Dom.Zero
	| Dom.Zero, Const(CInt64(i,_,_)) when i > Int64.zero ->
		Dom.NonZero
	| _else__________ ->
		Dom.lt v1 (eval_exp lenv e2)

let eval lenv exp =
	if Opts.path_check()
	then eval_exp lenv exp
	else Dom.Dunno

(*******************************************************************)
(* Refining the abstraction from control-flow tests and statements *)
(*******************************************************************)

let rec from_exp fnAbs lenv v exp =
	match CE.stripCastsOp exp with
	| (Lval _ as e)
	| (AddrOf _ as e) ->
		gen fnAbs lenv e v
	| UnOp(LNot,e,_) ->
		from_exp fnAbs lenv (not v) e
	| BinOp(Eq,(Lval _ as x),e,_)
	| BinOp(Eq,(AddrOf _ as x),e,_)
	| BinOp(Eq,e,(Lval _ as x),_)
	| BinOp(Eq,e,(AddrOf _ as x),_)
	->
		let lenv' = from_eq fnAbs lenv v x e in
		gen fnAbs lenv' exp v
	| BinOp(Ne,(Lval _ as x),e,_)
	| BinOp(Ne,(AddrOf _ as x),e,_)
	| BinOp(Ne,e,(Lval _ as x),_)
	| BinOp(Ne,e,(AddrOf _ as x),_)
	->
		let lenv' = from_ne fnAbs lenv v x e in
		gen fnAbs lenv' exp v
	| _else___ ->
		gen fnAbs lenv exp v

and from_eq fnAbs lenv v x = function
| Const(CInt64(i,_,_)) when i = Int64.zero ->
	gen fnAbs lenv x (not v) (* x == 0 or else x != 0 *)
| Const(CInt64(i,_,_)) when v && i <> Int64.zero ->
	gen fnAbs lenv x true (* x == i /\ i != 0 ==> x != 0 *)
| (Lval _ as y)
| (AddrOf _ as y) ->
	Dom.(match eval_exp lenv x, eval_exp lenv y with
	| Zero,Dunno when v     -> gen fnAbs lenv y false (* 0 == y *)
	| Dunno,Zero when v     -> gen fnAbs lenv x false (* x == 0 *)
	| Zero,Dunno when not v -> gen fnAbs lenv y true  (* 0 != y *)
	| Dunno,Zero when not v -> gen fnAbs lenv x true  (* x != 0 *)
	| __else_______________ -> lenv
	)
| __else______________ ->
	lenv

and from_ne fnAbs lenv v x = function
| Const(CInt64(i,_,_)) when i = Int64.zero ->
	gen fnAbs lenv x v (* x != 0 or else x == 0 *)
| Const(CInt64(i,_,_)) when not v && i <> Int64.zero ->
	gen fnAbs lenv x true (* x == i /\ i != 0 ==> x != 0 *)
| (Lval _ as y)
| (AddrOf _ as y) ->
	Dom.(match eval_exp lenv x, eval_exp lenv y with
	| Zero,Dunno when v     -> gen fnAbs lenv y true  (* 0 != y *)
	| Dunno,Zero when v     -> gen fnAbs lenv x true  (* x != 0 *)
	| Zero,Dunno when not v -> gen fnAbs lenv y false (* 0 == y *)
	| Dunno,Zero when not v -> gen fnAbs lenv x false (* x == 0 *)
	| __else_______________ -> lenv
	)
| __else______________ ->
	lenv

let kill_updated fnAbs lenv ef =
	E.(regions (filter is_writes ef))
	|> Regions.enum
	|> Enum.fold kill_by_region lenv

let from_set fna lenv ef lexp = function
| Const(CInt64(i,_,_)) ->
	gen fna lenv lexp (i <> Int64.zero)
| rexp ->
	match Dom.to_option (eval_exp lenv rexp) with
	| Some rexp_val -> gen fna lenv lexp rexp_val
	(* Otherwise kill the variable *)
	| None -> kill_updated fna lenv ef

(* FIXME: We can be more precise if we have the effects of the individual instructions. *)
let rec from_stmt fnAbs lenv ef = function
| [] -> lenv
| (Set(lv,e,_)::stmt') ->
	let lenv' = from_set fnAbs lenv ef (Lval lv) (CE.stripCastsOp e) in
	from_stmt fnAbs lenv' ef stmt'
| (Call(Some lv, Lval(Var f,NoOffset), [value;_expected], _)::stmt')
when f.vname = "__builtin_expect" ->
	let lenv' = from_set fnAbs lenv ef (Lval lv) (CE.stripCastsOp value) in
	from_stmt fnAbs lenv' ef stmt'
(* Otherwise (e.g. asm) kill any variable that has been updated. *)
| __else_____________________ ->
	kill_updated fnAbs lenv ef

let from_stmt fna lenv ef is =
	if Opts.path_check()
	then from_stmt fna lenv ef is
	else lenv

let from_test fnAbs lenv v exp =
	if Opts.path_check()
	then from_exp fnAbs lenv v exp
	else lenv

(********************************************)
(* Propagating facts through function calls *)
(********************************************)

open Option.Infix

let rec subst amap : exp -> exp option = function
| Const c
-> Some(Const c)
| Lval lv
-> Option.map (fun lv' -> Lval lv') (subst_lval amap lv)
| AddrOf lv
-> Option.map (fun lv' -> AddrOf lv') (subst_lval amap lv)
| CastE(ty,e)
-> Option.map (fun e' -> CastE(ty,e')) (subst amap e)
| BinOp(op,e1,e2,ty)
->
	subst amap e1 >>= fun e1' ->
	subst amap e2 >>= fun e2' ->
		Some(BinOp(op,e1',e2',ty))
| _else___
-> None

(** Given an l-value `a.b.c.d' it first tries to loop in `amap' all the possible
 * prefixes by popping one offset at a time, from `a.b.c.d' to `a.b'.
 *)
and subst_lval_prefix amap : lval -> (lval * offset list) option =
	let rec try_subst (acc : offset list) lv =
		match List.Exceptionless.assoc (Lval lv) amap with
		| Some y -> Some((Var y,NoOffset),acc)
		| None   ->
			match Cil.removeOffsetLval lv with
			| (lhost,_), NoOffset
			->
				Option.map
					(fun lhost' -> ((lhost',NoOffset),acc))
					(subst_lhost amap lhost)
			|       lv',     off1
			-> try_subst (off1::acc) lv'
	in try_subst []

and subst_lval amap lval : lval option =
	subst_lval_prefix amap lval >>= fun (lval1,rev_offsets) ->
		(* If substitution succeeds on some prefix, it applies the substitution
		 * to the popped offsets, and reconstructs the l-value.
		 *)
		Utils.Option.fold_left_break lval1 rev_offsets ~f:(fun lv os ->
				Option.map
					(fun os' -> Cil.addOffsetLval os' lv)
					(subst_offset amap os)
			)

and subst_lhost amap : lhost -> lhost option = function
| Var x when x.vglob -> Some (Var x)
| Var x ->
	let x_lv = Lval(Cil.var x) in
	List.Exceptionless.assoc x_lv amap >>= fun y -> Some (Var y)
| Mem e -> Option.map (fun e' -> Mem e') (subst amap e)

and subst_offset amap : offset -> offset option = function
| NoOffset -> Some NoOffset
| Field(fi,os) ->
	subst_offset amap os >>= fun os' -> Some (Field(fi,os'))
| Index(e,os) ->
	subst amap e >>= fun e' ->
	subst_offset amap os >>= fun os' ->
		Some (Index(e',os'))

let propagate fna formals actuals lenv : t =
	let amap = List.combine actuals formals in
	(* if e -> x, learn about `x' by evaluating `e' *)
	let lenv1 = List.fold_left (fun l (e,x) ->
			match Dom.to_option (eval_exp l e) with
			| None   -> l
			| Some b -> gen fna l (Lval(Cil.var x)) b
		) empty amap
	in
	(* if e -> x, try substituting `e' with `x' in previously known facts *)
	let lenv2 = ExpMap.fold (fun e v l ->
			match subst amap e with
			| Some e' -> gen fna l e' v
			| None    -> l
		) lenv.facts lenv1
	in
	lenv2
