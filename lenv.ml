open Batteries

open Type

open Cil

module VarMap    = Map.Make(Utils.Varinfo)
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
	(** Abstraction: x -> != 0 (true) | == 0 (false) | don't-know *)
	vfacts : bool VarMap.t;
	(** r -> x: if r is modified then forget anything you know about x *)
	vmonit : Cil.varinfo RegionMap.t;
}

let empty = { vfacts = VarMap.empty; vmonit = RegionMap.empty }

(** Record a fact `v' about some variable `x'.
 *
 * NB: For now, only local variables are tracked.
 *)
let gen fnAbs lenv x v =
	match FunAbs.find_var fnAbs x with
	| None    -> lenv
	| Some xz ->
		let r, _ = Shape.get_ref xz in
		{ vfacts = VarMap.add x v lenv.vfacts
		; vmonit = RegionMap.add r x lenv.vmonit
		}

let kill_by_var fnAbs lenv x =
	match FunAbs.find_var fnAbs x with
	| None    -> lenv
	| Some xz ->
		let r, _ = Shape.get_ref xz in
		{ vfacts = VarMap.remove x lenv.vfacts
		; vmonit = RegionMap.remove r lenv.vmonit
		}

let kill_by_region lenv r =
	match RegionMap.Exceptionless.find r lenv.vmonit with
	| None   -> lenv
	| Some x ->
		{ vfacts = VarMap.remove x lenv.vfacts
		; vmonit = RegionMap.remove r lenv.vmonit
		}

(***************************************************)
(* Evaluation of expressions under the abstraction *)
(***************************************************)

let rec eval lenv : exp -> Dom.t = function
| Lval (Var x,NoOffset) ->
	eval_var lenv x
| UnOp(LNot,e,_) ->
	Dom.lnot (eval lenv e)
| BinOp(Ne,Lval(Var x,NoOffset),e,_)
| BinOp(Ne,e,Lval(Var x,NoOffset),_) ->
	eval_ne lenv x e
| BinOp(Lt,Lval(Var x,NoOffset),e,_)
| BinOp(Gt,e,Lval(Var x,NoOffset),_) ->
	eval_lt lenv x e
| __else________________ ->
	Dom.Dunno

and eval_var_as_option lenv x =
	VarMap.Exceptionless.find x lenv.vfacts

and eval_var lenv x =
	Dom.of_option (eval_var_as_option lenv x)

and eval_ne lenv x = function
| Const(CInt64(i,_,_)) when i = Int64.zero ->
	Dom.(ne (eval_var lenv x) Zero)
| Const(CInt64(i,_,_)) when i <> Int64.zero ->
	Dom.(ne (eval_var lenv x) NonZero)
| Lval(Var y,NoOffset) ->
	Dom.ne (eval_var lenv x) (eval_var lenv y)
| __else______________ ->
	Dom.Dunno

and eval_lt lenv x = function
| Const(CInt64(i,_,_)) when i <= Int64.zero ->
	if eval_var lenv x = Dom.Zero
	then Dom.Zero
	else Dom.Dunno
| Const(CInt64(i,_,_)) when i > Int64.zero ->
	if eval_var lenv x = Dom.Zero
	then Dom.NonZero
	else Dom.Dunno
| Lval(Var y,NoOffset) ->
	Dom.lt (eval_var lenv x) (eval_var lenv y)
| _else__________ ->
	Dom.Dunno

(*******************************************************************)
(* Refining the abstraction from control-flow tests and statements *)
(*******************************************************************)

let rec from_test fnAbs lenv v = function
| Lval (Var x,NoOffset) ->
	gen fnAbs lenv x v
| UnOp(LNot,e,_) ->
	from_test fnAbs lenv (not v) e
(* | BinOp(Eq,e1,e2,_) -> *)
| BinOp(Ne,Lval(Var x,NoOffset),e,_)
| BinOp(Ne,e,Lval(Var x,NoOffset),_) ->
	from_test_ne fnAbs lenv v x e
| __else________________ ->
	lenv

and from_test_ne fnAbs lenv v x = function
| Const(CInt64(i,_,_)) when i = Int64.zero ->
	gen fnAbs lenv x v (* x != 0 or else x == 0 *)
| Const(CInt64(i,_,_)) when not v && i <> Int64.zero ->
	gen fnAbs lenv x true (* x == i /\ i != 0 ==> x != 0 *)
| Lval(Var y,NoOffset) ->
	Dom.(match eval_var lenv x, eval_var lenv y with
	| Zero,Dunno when v     -> gen fnAbs lenv y true  (* 0 != y *)
	| Dunno,Zero when v     -> gen fnAbs lenv x true  (* x != 0 *)
	| Zero,Dunno when not v -> gen fnAbs lenv y false (* 0 == y *)
	| Dunno,Zero when not v -> gen fnAbs lenv x false (* x == 0 *)
	| __else_______________ -> lenv
	)
| __else______________ ->
	lenv

let from_set fnAbs lenv ef x = function
| Const(CInt64(i,_,_)) ->
	gen fnAbs lenv x (i <> Int64.zero)
| Lval(Var y,NoOffset) when VarMap.mem y lenv.vfacts ->
	eval_var_as_option lenv y |> Option.map_default (fun y_val ->
		gen fnAbs lenv x y_val
		)
		lenv
(* Otherwise kill the variable *)
| __else______________ ->
	kill_by_var fnAbs lenv x

let from_stmt fnAbs lenv ef = function
| [Set((Var x,NoOffset),e,_)] ->
	from_set fnAbs lenv ef x (stripCasts e)
(* Otherwise kill any variable that has been updated. *)
| __else_____________________ ->
	E.(regions (filter is_writes ef))
	|> Regions.enum
	|> Enum.fold kill_by_region lenv
