
open Batteries

open Type

type name = string

type axiom = Axiom of name * shape scheme

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
		Vars.of_list ([Shape z1;Effect f1] @ regions [r1;r2;r3;r4;r5;r6;r7])
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
		Vars.of_list ([Effect f1;Shape z1] @ regions [r1;r2])
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
		Vars.of_list ([Effect f1;Shape z1] @ regions [r1;r2])
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
		Vars.of_list ([Effect f1;Shape z1] @ regions [r1;r2;r3;r4;r5])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1;arg2;arg3]
		; effects = f1
		; range   = Ptr(Ref(r2,Var z1))
		; varargs = false
	}))
	in
	Axiom("memcpy", Scheme.({vars; body}))

let spin_lock :axiom =
	let r0 = Region.meta() in
	let r1 = Region.bound() in
	let r2 = Region.bound() in
	let f1 = EffectVar.bound_with
		E.(just (locks r2) +. reads r1 +. reads r2 +. writes r2)
	in
	let arg1 = Shape.(Ref(r1,Ptr(Ref(r2,Bot)))) in
	let vars =
		let open Var in
		let regions = List.map (fun r -> Region r) in
		Vars.of_list ([Effect f1] @ regions [r1;r2])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1]
		; effects = f1
		; range   = Bot
		; varargs = false
	}))
	in
	Axiom("spin_lock", Scheme.({vars; body}))

let spin_unlock :axiom =
	let r0 = Region.meta() in
	let r1 = Region.bound() in
	let r2 = Region.bound() in
	let f1 = EffectVar.bound_with
		E.(just (unlocks r2) +. reads r1 +. reads r2 +. writes r2)
	in
	let arg1 = Shape.(Ref(r1,Ptr(Ref(r2,Bot)))) in
	let vars =
		let open Var in
		let regions = List.map (fun r -> Region r) in
		Vars.of_list ([Effect f1] @ regions [r1;r2])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1]
		; effects = f1
		; range   = Bot
		; varargs = false
	}))
	in
	Axiom("spin_unlock", Scheme.({vars; body}))

let spin_lock_irq :axiom =
	let r0 = Region.meta() in
	let r1 = Region.bound() in
	let r2 = Region.bound() in
	let f1 = EffectVar.bound_with
		E.(just (locks r2) +. reads r1
		   +. reads r2 +. writes r2
		   +. IrqsOff
		)
	in
	let arg1 = Shape.(Ref(r1,Ptr(Ref(r2,Bot)))) in
	let vars =
		let open Var in
		let regions = List.map (fun r -> Region r) in
		Vars.of_list ([Effect f1] @ regions [r1;r2])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1]
		; effects = f1
		; range   = Bot
		; varargs = false
	}))
	in
	Axiom("spin_lock_irq", Scheme.({vars; body}))

let spin_unlock_irq :axiom =
	let r0 = Region.meta() in
	let r1 = Region.bound() in
	let r2 = Region.bound() in
	let f1 = EffectVar.bound_with
		E.(just (unlocks r2) +. reads r1
		   +. reads r2 +. writes r2
		   +. IrqsOn
		)
	in
	let arg1 = Shape.(Ref(r1,Ptr(Ref(r2,Bot)))) in
	let vars =
		let open Var in
		let regions = List.map (fun r -> Region r) in
		Vars.of_list ([Effect f1] @ regions [r1;r2])
	in
	let body = Shape.(Ref(r0,Fun {
		  domain  = [arg1]
		; effects = f1
		; range   = Bot
		; varargs = false
	}))
	in
	Axiom("spin_unlock_irq", Scheme.({vars; body}))

let local_bh_enable :axiom =
	let r0 = Region.meta() in
	let f1 = EffectVar.bound_with E.(just BhsOn) in
	let vars = Vars.of_list [Var.Effect f1]
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
	let vars = Vars.of_list [Var.Effect f1]
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
	let tbl = Hashtbl.create 5 in
	add_axiom tbl assert_fail;
	add_axiom tbl malloc;
	add_axiom tbl free;
	add_axiom tbl memcpy;
	add_axiom tbl spin_lock;
	add_axiom tbl spin_unlock;
	add_axiom tbl spin_lock_irq;
	add_axiom tbl spin_unlock_irq;
	add_axiom tbl local_bh_enable;
	add_axiom tbl local_bh_disable;
	tbl

let find fn = Hashtbl.Exceptionless.find axiom_map fn
