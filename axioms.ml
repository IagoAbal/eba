
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
	Axiom("__assert_fail", {vars; body})

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
	Axiom("spin_lock", {vars; body})

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
	Axiom("spin_unlock", {vars; body})

(* Axiom table *)

let add_axiom tbl (Axiom(fn,sch)) = Hashtbl.add tbl fn sch

let axiom_map : (name,shape scheme) Hashtbl.t  =
	let tbl = Hashtbl.create 5 in
	add_axiom tbl assert_fail;
	add_axiom tbl spin_lock;
	add_axiom tbl spin_unlock;
	tbl

let find fn = Hashtbl.Exceptionless.find axiom_map fn
