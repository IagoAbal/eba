module Opts = Opts.Get

open Batteries

open Type

(** Read-Write worst-case assumption for a function parameter:
 *  ie. any reference may be read or written.
 *)
let def_rw_of_param pz :E.t =
	let open Shape in
	match pz with
	| Ref(r,z) ->
		let z_ef = fully_RW z in
		E.(weaken (z_ef +. reads r))
	| __else_______ ->
		Error.panic_with "def_rw_of_shape: not a ref shape"

(* TODO: Write in terns of a yet-to-be-written Shape fold *)
(* TODO: This is very specific to Linux ... *)
let def_lock_of_param ?(bound=3) pz :E.t =
	let open Shape in
	let open Cil in
	let rec loop d ef = function
	| Ref(r,Struct z)
		when z.sinfo.cname = "spinlock"
		  || z.sinfo.cname = "mutex"   ->
		E.(ef +. locks r +. unlocks r)
	| Ref(_,z)
	| Ptr z    -> loop d ef z
	| Struct z when d > 0 ->
		list_fields z |> List.fold_left (fun ef1 fz ->
			loop (d-1) ef1 fz.fshape
		) ef
	| __else___ ->
		E.none
	in
	E.weaken(loop bound E.none pz)

let mk_default_fp x xz :E.t =
	if Opts.externs_do_nothing()
	then E.none
	else
		let args,_,_ = Shape.get_fun xz in
		E.(
			(sum (List.map def_rw_of_param args))
			 +
			(sum (List.map def_lock_of_param args))
		)

(* Generate default axiom for an extern declaration.
 * NB: Only function axioms are generalized (value restriction).
 * NB: When generalizing axioms, we do not need to perform the
 *     usual checks to avoid variable capturing, all variables
 *     were just freshly created.
 *)
let mk_default x :Shape.t Scheme.t =
	let x_ty = Cil.(x.vtype) in
	if Cil.isFunctionType x_ty
	then
		let z = Shape.of_typ x_ty in
		let fp = mk_default_fp x z in
		let _,ff,_ = Shape.get_fun z in
		ignore (EffectVar.add_lb fp ff); (* in-place update of meta eff-var *)
		let z_fv =
			(** THINK but for now assume that if an extern declaration takes no
			 * argument but returns some object, then that object is always the
			 * same. This is a bit ad-hoc to deal with some Linux functions such
			 * as `get_current'.
			 *
			 * We should find a general solution that offers a good compromise
			 * when inferring the I/O relation of extern functions.
			 *)
			if Utils.is_zero_arg_proc x_ty
			then Vars.just_effect ff
			else Shape.fv_of z
		in
		let sch = Scheme.quantify z_fv z in
		Scheme.({sch with body = Shape.new_ref_to sch.body })
	else
		Scheme.of_shape (Shape.ref_of x_ty)

type name = string

type axiom = Axiom of name * shape scheme

let __builtin_unreachable :axiom =
	let r0 = Region.meta() in
	let f1 = EffectVar.bound_with
		E.(just noret)
	in
	let vars =
		QV.of_list [Var.Effect f1]
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = []
		; effects = f1
		; range   = Bot
		; varargs = false
	}))
	in
	Axiom("__builtin_unreachable", Scheme.({vars; body}))

(* TODO: This could be automatically inferred by recognizing GCC's `noreturn' attribute. *)
let assert_fail :axiom =
	let r0 = Region.meta() in
	let z1 = Shape.bound_var() in
	let r1 = Region.bound() in
	let r2 = Region.bound() in
	let r3 = Region.bound() in
	let r4 = Region.bound() in
	let r5 = Region.bound() in
	let r6 = Region.bound() in
	let r7 = Region.bound() in
	let f1 = EffectVar.bound_with
		E.(just noret +. reads r1 +. reads r2
		              +. reads r3 +. reads r4
		              +. reads r5
		              +. reads r6 +. reads r7
		)
	in
	let arg1 = Shape.(Ref(r1,Ptr(Ref(r2,Bot)))) in
	let arg2 = Shape.(Ref(r3,Ptr(Ref(r4,Bot)))) in
	let arg3 = Shape.(Ref(r5,Bot)) in
	let arg4 = Shape.(Ref(r6,Ptr(Ref(r7,Bot)))) in
	let vars =
		let open Var in
		(* TODO: Move as smart constructor for Type.Vars *)
		let regions = List.map (fun r -> Region r) in
		QV.of_list ([Shape z1;Effect f1] @ regions [r1;r2;r3;r4;r5;r6;r7])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1;arg2;arg3;arg4]
		; effects = f1
		; range   = Var z1
		; varargs = false
	}))
	in
	Axiom("__assert_fail", Scheme.({vars; body}))

let malloc :axiom =
	let r0 = Region.meta() in
	let r1 = Region.bound() in
	let r2 = Region.bound() in
	let z1 = Shape.bound_var() in
	let f1 = EffectVar.bound_with
		E.(none +. reads r1 +. allocs r2 +. uninits r2)
	in
	let arg1 = Shape.(Ref(r1,Bot)) in
	let vars =
		let open Var in
		let regions = List.map (fun r -> Region r) in
		QV.of_list ([Effect f1;Shape z1] @ regions [r1;r2])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1]
		; effects = f1
		; range   = Ptr(Ref(r2,Var z1))
		; varargs = false
	}))
	in
	Axiom("malloc", Scheme.({vars; body}))

let free :axiom =
	let r0 = Region.meta() in
	let r1 = Region.bound() in
	let r2 = Region.bound() in
	let z1 = Shape.bound_var() in
	let f1 = EffectVar.bound_with
		E.(none +. reads r1 +. frees r2 +. uninits r2)
	in
	let arg1 = Shape.(Ref(r1,Ptr(Ref(r2,Var z1)))) in
	let vars =
		let open Var in
		let regions = List.map (fun r -> Region r) in
		QV.of_list ([Effect f1;Shape z1] @ regions [r1;r2])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1]
		; effects = f1
		; range   = Bot
		; varargs = false
	}))
	in
	Axiom("free", Scheme.({vars; body}))

let memcpy :axiom =
	let r0 = Region.meta() in
	let r1 = Region.bound() in
	let r2 = Region.bound() in
	let r3 = Region.bound() in
	let r4 = Region.bound() in
	let r5 = Region.bound() in
	let z1 = Shape.bound_var() in
	let f1 = EffectVar.bound_with
		E.(none +. reads r1 +. writes r2 +. reads r3 +. reads r4)
	in
	let arg1 = Shape.(Ref(r1,Ptr(Ref(r2,Var z1)))) in
	let arg2 = Shape.(Ref(r3,Ptr(Ref(r4,Var z1)))) in
	let arg3 = Shape.(Ref(r5,Bot)) in
	let vars =
		let open Var in
		let regions = List.map (fun r -> Region r) in
		QV.of_list ([Effect f1;Shape z1] @ regions [r1;r2;r3;r4;r5])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1;arg2;arg3]
		; effects = f1
		; range   = Ptr(Ref(r2,Var z1))
		; varargs = false
	}))
	in
	Axiom("memcpy", Scheme.({vars; body}))

let local_bh_enable :axiom =
	let r0 = Region.meta() in
	let f1 = EffectVar.bound_with E.(just BhsOn) in
	let vars = QV.of_list [Var.Effect f1]
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = []
		; effects = f1
		; range   = Bot
		; varargs = false
	}))
	in
	Axiom("local_bh_enable", Scheme.({vars; body}))

let local_bh_disable :axiom =
	let r0 = Region.meta() in
	let f1 = EffectVar.bound_with E.(just BhsOff) in
	let vars = QV.of_list [Var.Effect f1]
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = []
		; effects = f1
		; range   = Bot
		; varargs = false
	}))
	in
	Axiom("local_bh_disable", Scheme.({vars; body}))

(* Axiom table *)

let add_axiom tbl (Axiom(fn,sch)) = Hashtbl.add tbl fn sch

let axiom_map : (name,shape scheme) Hashtbl.t  =
	let tbl = Hashtbl.create 11 in
	add_axiom tbl __builtin_unreachable;
	add_axiom tbl assert_fail;
	add_axiom tbl malloc;
	add_axiom tbl free;
	add_axiom tbl memcpy;
	add_axiom tbl local_bh_enable;
	add_axiom tbl local_bh_disable;
	tbl

(* TODO: Should check that the axiom is compatible with the function's signature. *)
let find f =
	let fn = Cil.(f.vname) in
	match Hashtbl.Exceptionless.find axiom_map fn with
	| Some ax -> ax
	| None    -> mk_default f

let getopt32_fp f _fz _pes pzs :E.t =
	let ef_on_param pz =
		let open Shape in
		match pz with
		| Ptr(Ref(r2,_)) ->
			E.(none +. writes r2)
		| _else_________________ ->
			E.none
	in
	if Cil.(f.vname = "getopt32")
	(* getopt32(char **argv, const char *applet_opts, ...) *)
	then pzs |> List.drop 2 |> List.map ef_on_param |> E.sum
	else E.none

let spin_lock_fp f _fz _pes pzs :E.t =
	if Cil.(f.vname = "spin_lock" || f.vname = "spin_lock_irq")
	(* void spin_lock(spinlock_t *lock) *)
	then
		let open Shape in
		match pzs with
		| [Ptr(Ref(r2,_))] when Cil.(f.vname = "spin_lock_irq")
		                   -> E.(none +. locks r2 +. IrqsOff)
		| [Ptr(Ref(r2,_))] -> E.(none +. locks r2)
		| _else___________ -> E.none
	else E.none

let spin_unlock_fp f _fz _pes pzs :E.t =
	if Cil.(f.vname = "spin_unlock" || f.vname = "spin_unlock_irq")
	(* void spin_unlock(spinlock_t *lock) *)
	then
		let open Shape in
		match pzs with
		| [Ptr(Ref(r2,_))] when Cil.(f.vname = "spin_unlock_irq")
		                   -> E.(none +. unlocks r2 +. IrqsOn)
		| [Ptr(Ref(r2,_))] -> E.(none +. unlocks r2)
		| _else___________ -> E.none
	else E.none

(* TODO: This is called on every function call, so it must be fast.
 * Right now, this is O(n) where n = number of footprint axioms. But any solution
 * should allow writing a single axiom for multiple functions at once: eg. when
 * those functions have similar signature and can be identified by a naming
 * convention. We could use a pair of hash-tables indexed by prefixes (ie. xyz_)
 * and suffixes (ie. _xyz) respectively. That may be enough to handle large
 * amounts of footprint axioms efficiently.
 *)
let footprint fe fz pes pzs =
	match fe with
	| Cil.Lval (Cil.Var f,Cil.NoOffset) ->
		E.sum [getopt32_fp f fz pes pzs
			  ;spin_lock_fp f fz pes pzs
			  ;spin_unlock_fp f fz pes pzs
			  ]
	| _other___________________________ ->
		E.none
