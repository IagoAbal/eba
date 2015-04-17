
open Batteries

(* THINK: Should create a common AST module ? *)

(* Shapes *)

module rec Shape : sig

	type t = Var of Var.shape
	 	  | Bot
		  | Ptr of t
		  | Fun of fun_shape
		  | Ref of Var.region * t

	and fun_shape =
		{ domain  : dom_shape
		; effects : Var.effect
		; range   : t
		}

	and dom_shape = t list

	val fv_of : t -> Vars.t

	val free_in : Var.t -> t -> bool

	val not_free_in : Var.t -> t -> bool

	(** Given [z] constructs [Ref (r,z)] with [r] fresh. *)
	val new_ref_to : t -> t

	(** A fresh shape variable [Var x]. *)
	val new_shape : unit -> t

	val zonk_shape : t -> t

	(** Shape of a given CIL type. *)
	val of_typ : Cil.typ -> t

	(** Reference shape to a given CIL type. *)
	val ref_of : Cil.typ -> t

	val get_fun : t -> dom_shape * Var.effect * t

	val var_subst : Var.t Subst.t -> t -> t

	end
	= struct

	type t = Var of Var.shape
		   | Bot
		   | Ptr of t
		   | Fun of fun_shape
		   | Ref of Var.region * t

	and fun_shape =
		{ domain  : dom_shape
		; effects : Var.effect
		; range   : t
		}

	and dom_shape = t list

	let rec fv_of :t -> Vars.t = function
		| Var x     -> Vars.singleton x
		| Bot       -> Vars.empty
		| Ptr z     -> fv_of z
		| Fun f     -> fv_of_fun_shape f
		| Ref (r,z) -> Vars.add r (fv_of z)

    and fv_of_shapes (zs :t list) :Vars.t =
    	let fvs = List.map fv_of zs in
    	List.fold_left Vars.union Vars.empty fvs

	and fv_of_fun_shape {domain; effects; range} =
		let dom_ftv = fv_of_shapes domain in
		let rng_ftv = fv_of range in
		Vars.add effects (Vars.union dom_ftv rng_ftv)

	let free_in a z = Vars.mem a (fv_of z)

	let not_free_in a z = not(free_in a z)

    let new_ref_to (z :t) :t =
        let r = Var.new_region() in
        Ref (r,z)

	let new_shape () : t =
		let meta_var = Var.new_shape_var() in
		Var meta_var

	let rec zonk_shape :t -> t = function
		| Var x     -> zonk_var x
		| Bot       -> Bot
		| Ptr z     -> Ptr (zonk_shape z)
		| Fun f     -> Fun (zonk_fun f)
		| Ref (r,z) -> let z' = zonk_shape z in
		               Ref(r,z')
	and zonk_var a :t =
		let az_opt = Var.read_shape_var a in
		match az_opt with
		| None -> Var a
		| Some z ->
		  let z' = zonk_shape z in
		  Var.write_shape_var a z';
		  z'

	and zonk_fun f =
		let { domain = dom; effects = ef; range = res } = f in
		{ domain  = zonk_dom dom
		; effects = ef
		; range   = zonk_shape res
		}

    and zonk_dom d = List.map zonk_shape d

	let rec of_typ (ty :Cil.typ) :t =
		match ty with
		| Cil.TVoid _
		| Cil.TInt _
		-> new_shape()
		| Cil.TFloat _
		-> Bot
		| Cil.TPtr(ty,_)
		| Cil.TArray(ty,_,_)
		-> Ptr (ref_of ty)
		| Cil.TFun(res,Some args,false,_)
		-> let domain = of_fun_args args in
		   let effects = Var.new_effect_var() in
		   let range = of_typ res in
		   Fun { domain; effects; range }
		| _ -> Error.not_implemented()

	and of_fun_args : _ -> dom_shape = function
		| []             -> []
		| (_,ty,_)::args -> ref_of ty :: of_fun_args args

    and ref_of ty :t =
    	let z = of_typ ty in
    	new_ref_to z

    let get_fun : t -> dom_shape * Var.effect * t
    	= function
    	| Fun {domain; effects; range}
    	-> domain, effects, range
    	| __other__
    	-> Error.panic()

    (* substitute variables with variables *)
    let rec var_subst (s :Var.t Subst.t) : t -> t
    	= function
    	| Var x    -> Var (Subst.on_var s x)
    	| Bot      -> Bot
    	| Ptr z    -> Ptr (var_subst s z)
    	| Fun fz   -> Fun (var_subst_fun s fz)
    	| Ref(r,z) -> Ref (Subst.on_var s r,var_subst s z)
    and var_subst_fun s = fun { domain; effects; range } ->
    	let d' = List.map (var_subst s) domain in
    	let f' = Subst.on_var s effects in
    	let r' = var_subst s range in
    	{ domain = d'; effects = f'; range = r' }

    end

(* Effects *)

and Effects : sig

	type mem_kind = Alloc | Free | Read | Write

	type e = Var of Var.effect
		   | Mem of mem_kind * Var.region

	type t

	val none : t

	val read : r:Var.region -> t

	val write : r:Var.region -> t

	val just_var : Var.t -> t

	val (+) : t -> t -> t

	val filter : (e -> bool) -> t -> t

	val var_subst : Var.t Subst.t -> t -> t

	end
	= struct

	type mem_kind = Alloc | Free | Read | Write

	type e = Var of Var.effect
		   | Mem of mem_kind * Var.region

	let mk_var x =
		assert (Var.is_effect x);
		Var x

	let mk_mem ~k ~r =
		assert (Var.is_region r);
		Mem(k,r)


	module EffectSet = Set.Make(
		struct
			type t = e
			let compare = Pervasives.compare
		end
		)

	type t = EffectSet.t

	let none = EffectSet.empty

	let just = EffectSet.singleton

	let read ~r = just (mk_mem Read r)

	let write ~r = just (mk_mem Write r)

	let just_var x = just (mk_var x)

	let add = EffectSet.add

	let (+.) es e = add e es

	let (+) = EffectSet.union

	let filter = EffectSet.filter

	let var_subst_e (s :Var.t Subst.t) :e -> e
		= function
		| Var x     -> Var (Subst.on_var s x)
		| Mem (k,r) -> Mem (k,Subst.on_var s r)

	let var_subst (s :Var.t Subst.t) :t -> t =
		EffectSet.map (var_subst_e s)

	end

(* Variables *)

and Var : sig

	type t = Bound of Uniq.t * kind
			 (* TODO: MetaEff of Uniq.t Uref.t * Effects.t ref
				We would avoid substitution of principal models,
				but we need to break these into separate files
				first.
			  *)
			 | MetaEff of Uniq.t Uref.t
			 | MetaReg of Uniq.t Uref.t
			 | MetaShp of Uniq.t * Shape.t option ref

	and kind = Shp | Eff | Reg

	and effect = t

	and shape = t

	and region = t

	val kind_of : t -> kind

	val is_effect : t -> bool

	val is_region : t -> bool

	val is_shape : t -> bool

	val fresh : k:kind -> t

	val uniq_of : t -> Uniq.t

	val compare : t -> t -> int

	val new_effect_var : unit -> Var.effect

	val new_region : unit -> Var.region

	val new_shape_var : unit -> Var.shape

	val of_bound : t -> t

	val of_bounds : t list -> t list

	val read_shape_var : Var.shape -> Shape.t option

	val write_shape_var : Var.shape -> Shape.t -> unit

	end
	= struct

	type t = Bound of Uniq.t * kind (* Should rename to Param ? *)
			 (* TODO MetaEff of Uniq.t Uref.t * Effects.t ref
				We would avoid substitution of principal models,
				but we need to break these into separate files
				first.
			  *)
			 | MetaEff of Uniq.t Uref.t
			 | MetaReg of Uniq.t Uref.t
			 | MetaShp of Uniq.t * Shape.t option ref

	and kind = Shp | Eff | Reg

	and effect = t

	and shape = t

	and region = t

	let kind_of = function
		| Bound(_,k) -> k
		| MetaEff _  -> Eff
		| MetaReg _  -> Reg
		| MetaShp _  -> Shp

	let is_effect x = kind_of x = Eff

	let is_region x = kind_of x = Reg

	let is_shape x = kind_of x = Shp

	let fresh ~k :t =
		Bound(Uniq.fresh(),k)

	let uniq_of :t -> Uniq.t = function
		| Bound (u,_)
		| MetaShp (u,_)
		-> u
		| MetaEff uref
		| MetaReg uref
		-> Uref.uget uref

	let compare x y = Pervasives.compare (uniq_of x) (uniq_of y)

    let new_effect_var () : Var.effect =
    	let uniq = Uniq.fresh() in
    	let uref = Uref.uref uniq in
    	MetaEff uref

    let new_region () : Var.region =
    	let uniq = Uniq.fresh() in
    	let uref = Uref.uref uniq in
    	MetaReg uref

    let new_shape_var () : Var.shape =
    	let uniq = Uniq.fresh() in
    	let uref = ref None in
    	MetaShp(uniq,uref)

    let of_bound :t -> t
    	= function
    	| Bound (u,Shp) -> new_shape_var()
    	| Bound (u,Eff) -> new_effect_var()
    	| Bound (u,Reg) -> new_region()
    	| __other__     -> Error.panic()

    let of_bounds :t list -> t list =
    	List.map of_bound

	let read_shape_var : Var.shape -> Shape.t option = function
		| MetaShp(_,zoref) -> !zoref
		| __else__         -> Error.panic()

	let write_shape_var x z =
		match x with
		| MetaShp(_,zoref) -> zoref := Some z
		| __else__         -> Error.panic()

	end

and Vars : sig
	include Set.S with type elt := Var.t
	end
	= Set.Make(Var)

and VarMap : sig
	include Map.S with type key := Var.t
	end
	= Map.Make(Var)

and Subst : sig

	type 'a t

	val mk : (Var.t * 'a) list -> 'a t

	val find : Var.t -> 'a t -> 'a option

	val on_var : Var.t t -> Var.t -> Var.t

	end
	= struct

	module VarMap = Map.Make(Var)

	type 'a t = 'a VarMap.t

	let mk : (Var.t * 'a) list -> 'a t = fun xs ->
		(* assert all variables are "Bound" *)
		VarMap.of_enum (List.enum xs)

	let find = VarMap.Exceptionless.find

	let on_var (s :Var.t t) (x :Var.t) :Var.t =
		Option.default x (find x s)

	end

module E = Effects

module type FVable = sig
	type t
	val fv_of : t -> Vars.t
end

type shape = Shape.t
type effects = Effects.t
type var = Var.t
type region = Var.region

(* Could remove scheme, since this is rank-1 polymorphism,
   we could just put "Bound"s and assume they are implicitly quantified. ?
   BUT they also appear in the constraints... and we need to subsitute there.
 *)
type 'a scheme =
		{ vars : Vars.t
		; body : 'a
		}

(* Unification *)

module Unify =
	struct

	open Shape

	(* In this application, these should be a panic().
	 * We assume the program type-checks.
	 *)
	exception Cannot_unify of shape * shape
	exception Occurs_check of Var.shape * shape

	let ok = ()

	let fail_cannot_unify s1 s2 = raise (Cannot_unify (s1,s2))

	let unify_regions r1 r2 =
		match (r1,r2) with
		| (Var.MetaReg uref1,Var.MetaReg uref2)
		-> Uref.unite uref1 uref2
		| __otherwise__
		-> Error.panic()

	let unify_effects ef1 ef2 =
		match (ef1,ef2) with
		| (Var.MetaEff uref1,Var.MetaEff uref2)
		-> Uref.unite uref1 uref2
		| __otherwise__
		-> Error.panic()

	let rec (=~) s1 s2 =
		match (s1,s2) with
		| (Var a,Var b)
		when a = b
		-> assert (Var.is_shape a);
		   assert (Var.is_shape b);
		   ok
		| (Var a,___)
		-> unify_var a s2
		| (___,Var b)
		-> unify_var b s1
		| (Bot,Bot)
		-> ok
		| (Ptr x,Ptr y)
		-> x =~ y
		| (Fun f1,Fun f2)
		-> unify_fun f1 f2
		| (Ref(r,x),Ref(s,y))
		-> unify_regions r s;
		   x =~ y
		| __otherwise__
		-> fail_cannot_unify s1 s2

    and unify_fun f1 f2 =
    	let unify_dom d1 d2 =
    		try List.iter2 (=~) d1 d2
    		with Invalid_argument _ -> Error.panic()
    	in
    	let { domain = dom1; effects = ef1; range = res1 } = f1 in
    	let { domain = dom2; effects = ef2; range = res2 } = f2 in
    	unify_dom dom1 dom2;
    	unify_effects ef1 ef2;
    	res1 =~ res2

	and unify_var a z =
		let azopt = Var.read_shape_var a in
		match azopt with
		| None -> let z' = Shape.zonk_shape z in
		          unify_unbound_var a z'
		| Some z1 -> z1 =~ z

	and unify_unbound_var a = function
		| Var b when a = b
		-> ok
		| z when Shape.free_in a z
		-> raise(Occurs_check(a,z))
		| z
		-> Var.write_shape_var a z

	(* z =~ ptr z1 *)
	let match_ptr_shape z : shape =
		match z with
		| Ptr z1 -> z1
		| Var a  -> let z1 = Shape.new_shape() in
					unify_var a (Ptr z1);
					z1
		| ______ -> Error.panic()

	(* z =~ ref (r,z1) *)
	let match_ref_shape (z :shape) :Var.region * shape =
		match z with
		| Ref (r,z1) -> r, z1
		| Var a      ->
			let z1 = Shape.new_shape() in
			let r = Var.new_region() in
			unify_var a (Ref (r,z1));
			r, z1
		| __________ -> Error.panic()

	(* This one is only needed when we get structs ? *)
	(* TODO: If unification fails,
	   what about the variables that were unified already? *)
    let match_shape_with_typ (z1 :shape) (ty :Cil.typ) :shape =
    	let z2 = Shape.of_typ ty in
    	try
    		z1 =~ z2;
    		z1
    	with Cannot_unify _ -> z2 (* Oops, unsafe analysis... *)

	end

(* Subeffecting Constraints *)

module Constraints =
	struct

	open Var

	type k = Constraint of Var.effect * effects

    type t = k list

    (* TODO: Some sanity check here? *)
    let mk_constraint x ef : k = Constraint (x,ef)

    let none :t = []

    let add x ef (ks :t) :t = mk_constraint x ef :: ks

    let (+) ks1 ks2 = ks1 @ ks2

    let partition (xs :Vars.t) :t -> t * t =
    	List.partition (fun (Constraint (x,_)) -> Vars.mem x xs)

    let var_subst_k (s :Var.t Subst.t)
    	= fun (Constraint (x,f)) ->
    	Constraint(Subst.on_var s x,E.var_subst s f)

    let var_subst (s :Var.t Subst.t) :t -> t =
    	List.map (var_subst_k s)

    let principal : t -> Effects.t Subst.t =
    	Error.not_implemented()

    (* With Substable we can use first class modules... ?
       subst_on : (module Substable with type t = a, s = Effects.t) -> a -> a

       principal m_effects_subst k f
     *)
    let principal_effects (k :t) : Effects.t -> Effects.t =
    	let _s = principal k in
    	Error.not_implemented()

	end

module K = Constraints
