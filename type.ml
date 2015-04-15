
open Batteries

(* TODO: Organize into different files
 *)

(* Types *)

type var = Bound of Uniq.t * kind (* Should rename to Param ? *)
		 (* TODO MetaEff of Uniq.t Uref.t * Effects.t ref
		    We would avoid substitution of principal models,
		    but we need to break these into separate files
		    first.
		  *)
         | MetaEff of Uniq.t Uref.t
         | MetaReg of Uniq.t Uref.t
         | MetaShp of Uniq.t * shape option ref

and kind = Shp | Eff | Reg

and effect_var = var

and shape_var = var

and region = var

and shape = Var of shape_var
          | Bot
          | Ptr of shape
          | Fun of fun_shape
          | Ref of region * shape

and fun_shape =
    { domain  : dom_shape
    ; effects : effect_var
    ; range   : shape
    }

and dom_shape = shape list

(* Variables *)

module Var =
	struct

	type t = var

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

	let uniq_of :var -> Uniq.t = function
		| Bound (u,_)
		| MetaShp (u,_)
		-> u
		| MetaEff uref
		| MetaReg uref
		-> Uref.uget uref

	let compare x y = Pervasives.compare (uniq_of x) (uniq_of y)

	end

module Vars = Set.Make(Var)

(* Could remove scheme, since this is rank-1 polymorphism,
   we could just put "Bound"s and assume they are implicitly quantified. ?
   BUT they also appear in the constraints... and we need to subsitute there.
 *)
type 'a scheme =
		{ vars : Vars.t
		; body : 'a
		}

module VarMap = Map.Make(Var)

module FV =
	struct

	let rec of_shape :shape -> Vars.t = function
		| Var x     -> Vars.singleton x
		| Bot       -> Vars.empty
		| Ptr z     -> of_shape z
		| Fun f     -> of_fun_shape f
		| Ref (r,z) -> Vars.add r (of_shape z)

    and of_shapes (zs :shape list) :Vars.t =
    	let fvs = List.map of_shape zs in
    	List.fold_left Vars.union Vars.empty fvs

	and of_fun_shape {domain; effects; range} =
		let dom_ftv = of_shapes domain in
		let rng_ftv = of_shape range in
		Vars.add effects (Vars.union dom_ftv rng_ftv)

	let free_in a z = Vars.mem a (of_shape z)

	let not_free_in a z = not(free_in a z)

	module type FVable = sig
		type t
		val fv_of : t -> Vars.t
	end

	end

module Subst =
	struct

	module VarMap = Map.Make(Var)

	type 'a t = 'a VarMap.t

	let mk : (Var.t * 'a) list -> 'a t = fun xs ->
		(* assert all variables are "Bound" *)
		VarMap.of_enum (List.enum xs)

	let find = VarMap.Exceptionless.find

	let on_var (s :Var.t t) (x :Var.t) :Var.t =
		Option.default x (find x s)

	end

module Meta =
	struct

    let new_effect_var () : effect_var =
    	let uniq = Uniq.fresh() in
    	let uref = Uref.uref uniq in
    	MetaEff uref

    let new_region () : region =
    	let uniq = Uniq.fresh() in
    	let uref = Uref.uref uniq in
    	MetaReg uref

    let new_shape_var () : shape_var =
    	let uniq = Uniq.fresh() in
    	let uref = ref None in
    	MetaShp(uniq,uref)

    let of_bound :var -> var
    	= function
    	| Bound (u,Shp) -> new_shape_var()
    	| Bound (u,Eff) -> new_effect_var()
    	| Bound (u,Reg) -> new_region()
    	| __other__     -> Error.panic()

    let of_bounds :var list -> var list =
    	List.map of_bound

    let new_ref_to (z :shape) :shape =
        let r = new_region() in
        Ref (r,z)

	let new_shape () : shape =
		let meta_var = new_shape_var() in
		Var meta_var

	let read_shape_var : shape_var -> shape option = function
		| MetaShp(_,zoref) -> !zoref
		| __else__         -> Error.panic()

	let write_shape_var x z =
		match x with
		| MetaShp(_,zoref) -> zoref := Some z
		| __else__         -> Error.panic()

	let rec zonk_shape :shape -> shape = function
		| Var x     -> zonk_var x
		| Bot       -> Bot
		| Ptr z     -> Ptr (zonk_shape z)
		| Fun f     -> Fun (zonk_fun f)
		| Ref (r,z) -> let z' = zonk_shape z in
		               Ref(r,z')

	and zonk_var a :shape =
		let az_opt = read_shape_var a in
		match az_opt with
		| None -> Var a
		| Some z ->
		  let z' = zonk_shape z in
		  write_shape_var a z';
		  z'

	and zonk_fun f =
		let { domain = dom; effects = ef; range = res } = f in
		{ domain  = zonk_dom dom
		; effects = ef
		; range   = zonk_shape res
		}

    and zonk_dom d = List.map zonk_shape d

	end

(* Shapes *)

module Shape =
	struct

	let rec of_typ (ty :Cil.typ) :shape =
		match ty with
		| Cil.TVoid _
		| Cil.TInt _
		-> Meta.new_shape()
		| Cil.TFloat _
		-> Bot
		| Cil.TPtr(ty,_)
		| Cil.TArray(ty,_,_)
		-> Ptr (ref_of ty)
		| Cil.TFun(res,Some args,false,_)
		-> let domain = of_fun_args args in
		   let effects = Meta.new_effect_var() in
		   let range = of_typ res in
		   Fun { domain; effects; range }
		| _ -> raise Error.Not_implemented

	and of_fun_args : _ -> dom_shape = function
		| []             -> []
		| (_,ty,_)::args -> ref_of ty :: of_fun_args args

    and ref_of ty :shape =
    	let z = of_typ ty in
    	Meta.new_ref_to z

    let get_fun : shape -> dom_shape * effect_var * shape
    	= function
    	| Fun {domain; effects; range}
    	-> domain, effects, range
    	| __other__
    	-> Error.panic()

    (* substitute variables with variables *)
    let rec var_subst (s :Var.t Subst.t) : shape -> shape
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

(* Unification *)

module Unify =
	struct

	(* In this application, these should be a panic().
	 * We assume the program type-checks.
	 *)
	exception Cannot_unify of shape * shape
	exception Occurs_check of shape_var * shape

	let ok = ()

	let fail_cannot_unify s1 s2 = raise (Cannot_unify (s1,s2))

	let unify_regions r1 r2 =
		match (r1,r2) with
		| (MetaReg uref1,MetaReg uref2)
		-> Uref.unite uref1 uref2
		| __otherwise__
		-> Error.panic()

	let unify_effects ef1 ef2 =
		match (ef1,ef2) with
		| (MetaEff uref1,MetaEff uref2)
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
		let azopt = Meta.read_shape_var a in
		match azopt with
		| None -> let z' = Meta.zonk_shape z in
		          unify_unbound_var a z'
		| Some z1 -> z1 =~ z

	and unify_unbound_var a = function
		| Var b when a = b
		-> ok
		| z when FV.free_in a z
		-> raise(Occurs_check(a,z))
		| z
		-> Meta.write_shape_var a z

	(* z =~ ptr z1 *)
	let match_ptr_shape z : shape =
		match z with
		| Ptr z1 -> z1
		| Var a  -> let z1 = Meta.new_shape() in
					unify_var a (Ptr z1);
					z1
		| ______ -> Error.panic()

	(* z =~ ref (r,z1) *)
	let match_ref_shape (z :shape) :region * shape =
		match z with
		| Ref (r,z1) -> r, z1
		| Var a      ->
			let z1 = Meta.new_shape() in
			let r = Meta.new_region() in
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

(* Effects *)

module Effects =
	struct

	type mem_kind = Alloc | Free | Read | Write

	type e = Var of effect_var
		   | Mem of mem_kind * region

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

module E = Effects

(* Subeffecting Constraints *)

module Constraints =
	struct

	type k = Constraint of effect_var * Effects.t

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
