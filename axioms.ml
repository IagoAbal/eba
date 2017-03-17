module Opts = Opts.Get

open Batteries

open Type

(** Blindly trusted axioms. *)
module Blind : sig

	val find  : Cil.varinfo -> shape scheme option

end = struct

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

	let find f =
		let fn = Cil.(f.vname) in
		Hashtbl.Exceptionless.find axiom_map fn

end

module Partial : sig

	val load_axioms : unit -> unit

	val find : Cil.varinfo -> shape -> E.t option

	val mk_default : Cil.varinfo -> shape scheme

end = struct
	(* TODO: Allow matching by prefix/suffix *)
	(* TODO: Allow multiple matches. *)
	(* TODO: Recognize GCC's `noreturn' attribute. *)

	open Shape

	type mk_effects = Cil.varinfo -> shape -> shape list -> Effects.t

	type axiom = Axiom of name * mk_effects

	let ax_mutex_lock :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. locks r2 + weaken (just Sleep))
			| _else____________ ->
				E.none
		in
		Axiom("mutex_lock", mk)

	let ax_mutex_lock_nested :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)));Ref(r3,_)] ->
				E.(just (reads r1) +. locks r2 +. reads r3 + weaken (just Sleep))
			| _else____________ ->
				E.none
		in
		Axiom("mutex_lock_nested", mk)

	let ax_mutex_lock_interruptible_nested :axiom =
		let Axiom(_,mk) = ax_mutex_lock_nested in
		Axiom("mutex_lock_interruptible_nested", mk)

	let ax_mutex_unlock :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. unlocks r2)
			| _else____________ ->
				E.none
		in
		Axiom("mutex_unlock", mk)

	let ax_spin_lock :axiom = (* OLD Linux versions *)
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. locks r2)
			| _else____________ ->
				E.none
		in
		Axiom("_spin_lock", mk)

	let ax_raw_spin_lock :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. locks r2)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_spin_lock", mk)

	let ax_spin_unlock :axiom = (* OLD Linux versions *)
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. unlocks r2)
			| _else____________ ->
				E.none
		in
		Axiom("_spin_unlock", mk)

	let ax_raw_spin_unlock :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. unlocks r2)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_spin_unlock", mk)

	let ax__raw_spin_unlock :axiom =
		let Axiom(_,mk) = ax_raw_spin_unlock in
		Axiom("__raw_spin_unlock", mk)

	let ax__raw_spin_trylock :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				(* It *may* lock.
				 * THINK: Path-sensitive effects?
				 *)
				E.(just (reads r1) + weaken (just (locks r2)))
			| _else____________ ->
				E.none
		in
		Axiom("__raw_spin_trylock", mk)

	let ax_raw_read_lock :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. locks r2)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_read_lock", mk)

	let ax_raw_read_unlock :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. unlocks r2)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_read_unlock", mk)

	let ax__raw_read_unlock :axiom =
		let Axiom(_,mk) = ax_raw_read_unlock in
		Axiom("__raw_read_unlock", mk)

	let ax_raw_spin_lock_irq :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. locks r2 +. IrqsOff)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_spin_lock_irq", mk)

	let ax_raw_spin_unlock_irq :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. unlocks r2 +. IrqsOn)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_spin_unlock_irq", mk)

	let ax__raw_spin_unlock_irq :axiom =
		let Axiom(_,mk) = ax_raw_spin_unlock_irq in
		Axiom("__raw_spin_unlock_irq", mk)

	let ax_raw_spin_lock_irqsave :axiom =
		let Axiom(_,mk) = ax_raw_spin_lock_irq in
		Axiom("_raw_spin_lock_irqsave",mk)

	let ax_raw_spin_unlock_irqrestore :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)));Ref(r3,_)] ->
				E.(just (reads r1) +. unlocks r2 +. IrqsOn +. reads r3)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_spin_unlock_irqrestore",mk)

	let ax_spin_lock_bh :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. locks r2 +. BhsOff)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_spin_lock_bh", mk)

	let ax_spin_unlock_bh :axiom =
		let mk _fn _z = function
			| [Ref(r1,Ptr(Ref(r2,_)))] ->
				E.(just (reads r1) +. unlocks r2 +. BhsOn)
			| _else____________ ->
				E.none
		in
		Axiom("_raw_spin_unlock_bh", mk)

	let add_axiom tbl (Axiom(fn,mk)) = Hashtbl.add tbl fn mk

	let axiom_map : (name,mk_effects) Hashtbl.t  =
		Hashtbl.create 23

	let load_axioms() =
		let tbl = axiom_map in
		add_axiom tbl ax_spin_lock;
		add_axiom tbl ax_raw_spin_lock;
		add_axiom tbl ax_spin_unlock;
		add_axiom tbl ax_raw_spin_unlock;
		add_axiom tbl ax__raw_spin_unlock;
		add_axiom tbl ax__raw_spin_trylock;
		add_axiom tbl ax_raw_spin_lock_irq;
		add_axiom tbl ax_raw_spin_unlock_irq;
		add_axiom tbl ax__raw_spin_unlock_irq;
		add_axiom tbl ax_raw_spin_lock_irqsave;
		add_axiom tbl ax_raw_spin_unlock_irqrestore;
		add_axiom tbl ax_spin_lock_bh;
		add_axiom tbl ax_spin_unlock_bh;
		if Opts.all_lock_types()
		then begin
			add_axiom tbl ax_mutex_lock;
			add_axiom tbl ax_mutex_lock_nested;
			add_axiom tbl ax_mutex_lock_interruptible_nested;
			add_axiom tbl ax_mutex_unlock;
			add_axiom tbl ax_raw_read_lock;
			add_axiom tbl ax_raw_read_unlock;
			add_axiom tbl ax__raw_read_unlock;
		end

	let find x xz =
		match Hashtbl.Exceptionless.find axiom_map Cil.(x.vname) with
		| Some mk ->
			let args,_,res = Shape.get_fun xz in
			Some (mk x res args)
		| None ->
			None

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

	(* TODO: Write in terms of a yet-to-be-written Shape fold. *)
	(* TODO: This is very specific to Linux ... *)
	let def_lock_of_param ?(bound=3) pz :E.t =
		let open Shape in
		let open Cil in
		let rec loop d ef = function
		| Ref(r,Struct z)
		when z.sinfo.cname = "spinlock" || z.sinfo.cname = "mutex" ->
			E.(ef +. locks r +. unlocks r)
		| Struct z when d > 0 ->
			list_fields z |> List.fold_left (fun ef1 fz ->
				loop (d-1) ef1 fz.fshape
			) ef
		| Ref(_,z)
		| Ptr z     -> loop d ef z
		| __else___ ->
			E.none
		in
		E.weaken(loop bound E.none pz)

	let mk_default_fp x xz :E.t =
		match find x xz with
		| Some ef ->
			ef
 		| None -> (* We know nothing about the extern, so we guess somethihg. *)
			if Opts.externs_do_nothing()
			then E.none
			else
				let args,_,_ = Shape.get_fun xz in
				E.(sum (List.map def_rw_of_param args)
				+ sum (List.map def_lock_of_param args)
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
			Constraints.add_to_fun z fp;
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
				then
					let _,ff,_ = Shape.get_fun z in
					Vars.just_effect ff
				else Shape.fv_of z
			in
			let sch = Scheme.quantify z_fv z in
			Scheme.({sch with body = Shape.new_ref_to sch.body })
		else
			Scheme.of_shape (Shape.ref_of x_ty)

end

(** Axioms applied on application site.
 *
 * Useful for variadic functions and (potentially) when effects depend on the
 * values passed to certain parameters.
 *)
module Onsite = struct

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

end

let load_axioms () =
	Partial.load_axioms()

(* TODO: Should check that the axiom is compatible with the function's signature. *)
let find f =
	match Blind.find f with
	| Some ax -> ax
	| None    -> Partial.mk_default f

let find_partial f fz = Option.(Partial.find f fz |? E.none)

(* TODO: This is called on every function call, so it must be fast.
 * Right now, this is O(n) where n = number of footprint axioms. But any solution
 * should allow writing a single axiom for multiple functions at once: eg. when
 * those functions have similar signature and can be identified by a naming
 * convention. We could use a pair of hash-tables indexed by prefixes (ie. xyz_)
 * and suffixes (ie. _xyz) respectively. That may be enough to handle large
 * amounts of footprint axioms efficiently.
 *)
let footprint fe fz pes pzs =
	let open Onsite in
	match fe with
	| Cil.Lval (Cil.Var f,Cil.NoOffset) ->
		E.sum [getopt32_fp f fz pes pzs
			  ]
	| _other___________________________ ->
		E.none
