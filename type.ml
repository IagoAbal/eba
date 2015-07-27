
open Batteries

(* Shapes *)

(* TODO: Smart constructors for shapes *)

module rec Shape : sig

	type t = Var of var
	       | Bot
	       | Ptr of t
	       | Fun of fun_shape
	       | Ref of Region.t * t

	and var

	and fun_shape =
		{ domain  : args
		(** The computational effects of a function are always
		 *  defined by an effect variable. This variable has
		 *  subeffecting constraints attached.
		 *)
		; effects : EffectVar.t
		; range   : result
		; varargs : bool
		}

	and args = t list

	and result = t

	val fv_of : t -> Vars.t

	val free_in : Var.t -> t -> bool

	val not_free_in : Var.t -> t -> bool

	(* THINK: Some of these new_ may better be named fresh_ ? *)

	(** Given [z] constructs [Ref (r,z)] with [r] fresh. *)
	val new_ref_to : t -> t

	val bound_var : unit -> var

	val meta_var : unit -> var

	(** A fresh shape variable [Var x]. *)
	val fresh : unit -> t

	(** A fresh pointer-shape [ptr ref[r] z] *)
	val fresh_ptr_to : t -> t

	val uniq_of : var -> Uniq.t

	val is_meta : var -> bool

	val read_var : var -> t option

	val write_var : var -> t -> unit

	val zonk : t -> t

	(** Shape of a given CIL type. *)
	val of_typ : Cil.typ -> t

	(** Reference shape to a given CIL type. *)
	val ref_of : Cil.typ -> t

	(** Shape from [varinfo]'s type. *)
	val of_varinfo : Cil.varinfo -> t

	val is_fun : t -> bool

    (* THINK: Should it be Unify.match_fun ? *)
	val get_fun : t -> args * EffectVar.t * result
	val get_ref_fun : t -> args * EffectVar.t * result

	val vsubst_var : Var.t Subst.t -> var -> var

	val vsubst : Var.t Subst.t -> t -> t

	(** Fold over shapes
	 *
	 * Cons:
	 * - Performance penalty (may improve with the compiler).
	 * Pros:
	 * - Reduce boilerplate.
	 * - Less dependent on shapes representation.
	 *)
	type 'a fold = {
		var : var -> 'a ;
		bot : 'a ;
		ptr : 'a -> 'a ;
		fn  : 'a * 'a list * bool * EffectVar.t -> 'a ;
		rf  : Region.t -> 'a -> 'a
	}

	val fold : 'a fold -> t -> 'a

	val regions_in : t -> Regions.t

	val pp_var : var -> PP.doc

	val pp_fun : fun_shape -> PP.doc

	val pp : t -> PP.doc

	val to_string : t -> string

	end
	= struct

	type t = Var of var
	       | Bot
	       | Ptr of t
	       | Fun of fun_shape
	       | Ref of Region.t * t

	and var = Bound of Uniq.t
	        | Meta of Uniq.t * t Meta.t

	and fun_shape =
		{ domain  : args
		; effects : EffectVar.t
		; range   : result
		; varargs : bool
		}

	and args = t list

	and result = t

	let rec fv_of :t -> Vars.t = function
		| Var a     -> fv_of_var a
		| Bot       -> Vars.none
		| Ptr z     -> fv_of z
		| Fun f     -> fv_of_fun f
		| Ref (r,z) -> Vars.add (Var.Region r) (fv_of z)

	and fv_of_var :var -> Vars.t = function
		(* surely, if it's bound then it cannot be free :-) *)
		| Bound _          -> Vars.none
		(* THINK: we go into meta variables to be more robust, should we? *)
		| Meta (_,ma) as a ->
			let fvs = Meta.fv_with fv_of ma in
			Vars.add (Var.Shape a) fvs

    and fv_of_list (zs :t list) :Vars.t =
    	let fvs = List.map fv_of zs in
    	List.fold_left Vars.union Vars.empty fvs

	and fv_of_fun {domain; effects; range} =
		let dom_fv = fv_of_list domain in
		let res_fv = fv_of range in
		Vars.add (Var.Effect effects) (Vars.union dom_fv res_fv)

	let free_in a z = Vars.mem a (fv_of z)

	let not_free_in a z = not(free_in a z)

    let new_ref_to (z :t) :t =
        let r = Region.meta() in
        Ref (r,z)

	let bound_var () :var =
		let id = Uniq.fresh() in
		Bound id

	let meta_var () :var =
		let id = Uniq.fresh() in
		let mz = Meta.fresh() in
		Meta(id,mz)

	let fresh () :t =
		Var(meta_var())

	let fresh_ptr_to z :t =
		let r = Region.meta() in
		Ptr(Ref(r,z))

	let uniq_of = function
		| Meta(u,_) -> u
		| Bound u   -> u

	let is_meta = function
		| Meta  _ -> true
		| Bound _ -> false

	let read_var : Shape.var -> Shape.t option = function
		| Meta(_,mz) -> Meta.read mz
		| Bound _    -> Error.panic_with("cannot read bound shape variable")

	let write_var x z =
		 match x with
		| Meta(_,mz) -> Meta.write mz z
		| Bound _    -> Error.panic_with("cannot write bound shape variable")

	let rec zonk :t -> t = function
		| Var x     -> zonk_var x
		| Bot       -> Bot
		| Ptr z     -> Ptr (zonk z)
		| Fun f     -> Fun (zonk_fun f)
		| Ref (r,z) -> let r' = Region.zonk r in
		               let z' = zonk z in
		               Ref(r',z')
	and zonk_var :var -> t = function
		| Bound _ as a ->
			Var a
		| Meta(_,mz) as z ->
			Option.(Meta.zonk_with zonk mz |? Var z)
	and zonk_fun f =
		{ domain  = zonk_dom f.domain
		; effects = EffectVar.zonk f.effects
		; range   = zonk f.range
		; varargs = f.varargs
		}
    and zonk_dom d = List.map zonk d

	(* THINK: on typesig instead ? *)
	let rec of_typ (ty :Cil.typ) :t =
		match ty with
		| Cil.TVoid _
		| Cil.TInt _
		(* Enumerations are treated as integers *)
		| Cil.TEnum _ ->
			fresh()
		| Cil.TFloat _
		-> Bot
		| Cil.TPtr(ty,_)
		| Cil.TArray(ty,_,_)
		-> Ptr (ref_of ty)
		| Cil.TFun(res,args_opt,varargs,_) ->
			let args = Option.(args_opt |? []) in
			let domain = of_args args in
			let effects = EffectVar.meta() in
			let range = of_typ res in
			Fun { domain; effects; range; varargs }
		| Cil.TNamed (ti,_) -> of_typ Cil.(ti.ttype)
		| Cil.TBuiltin_va_list _ ->
			Bot
		| _ ->
			Log.error "Type not supported\n"; (* TODO: Cil.d_type ty *)
			Error.not_implemented()

	and of_args : _ -> args = function
		| []             -> []
		| (_,ty,_)::args -> ref_of ty :: of_args args

    and ref_of ty :t =
    	let z = of_typ ty in
    	new_ref_to z

	let of_varinfo x = ref_of Cil.(x.vtype)

	let is_fun = function
		| Fun _ -> true
		| _else -> false

    let get_fun : t -> args * EffectVar.t * t = function
    	| Fun {domain; effects; range} ->
			domain, effects, range
    	| __other__ ->
			Log.error "%s is not a function shape\n" (Shape.to_string __other__);
			Error.panic()

	let get_ref_fun = function
		| Ref(_,z)  -> get_fun z
		| __other__ ->
			Log.error "%s is not a ref-to-function shape\n" (Shape.to_string __other__);
			Error.panic()

    (** Substitute variables with variables.
	 *
	 * To substitute variables with arbitrary shapes, simply
	 * substite with meta-variables and zonk.
	 *)
    let rec vsubst s : t -> t
    	= function
    	| Var a    -> Var(vsubst_var s a)
    	| Bot      -> Bot
    	| Ptr z    -> Ptr (vsubst s z)
    	| Fun fz   -> Fun (vsubst_fun s fz)
    	| Ref(r,z) -> Ref (Region.vsubst s r,vsubst s z)
	and vsubst_var s a =
		let x = Var.Shape a in
		Var.to_shape (Subst.find_default x x s)
    and vsubst_fun s = fun { domain; effects; range; varargs } ->
    	let d' = List.map (vsubst s) domain in
    	let f' = EffectVar.vsubst s effects in
    	let r' = vsubst s range in
    	{ domain = d'; effects = f'; range = r'; varargs }

	type 'a fold = {
		var : var -> 'a ;
		bot : 'a ;
		ptr : 'a -> 'a ;
		fn  : 'a * 'a list * bool * EffectVar.t -> 'a ;
		rf  : Region.t -> 'a -> 'a
	}

	let rec fold f = function
    	| Var a    -> f.var a
    	| Bot      -> f.bot
    	| Ptr z    -> f.ptr (fold f z)
    	| Fun fz   -> f.fn (fold_fun f fz)
    	| Ref(r,z) -> f.rf r (fold f z)
	and fold_fun f { domain; effects; range; varargs } =
		let v_range = fold f range in
		let v_domain = fold_args f domain in
		v_range, v_domain, varargs, effects
	and fold_args f args = args |> List.map (fold f)

	let regions_in =
		let f = {
			  var = const Regions.none
			; bot = Regions.none
			; ptr = identity
			; fn  = (fun (rs,rss,_,_) -> Regions.sum (rs::rss))
			; rf  = fun r rs -> Regions.add r rs
		} in fold f

	let rec pp = function
		| Var x    -> pp_var x
		| Bot      -> PP.(!^ "_|_")
		| Ptr z    -> PP.(!^ "ptr" + space + pp z)
		| Fun fz   -> pp_fun fz
		| Ref(r,z) -> PP.(!^ "ref" + brackets(Region.pp r) + space + pp z)
	and pp_var a =
		let vt_str = if is_meta a then "?" else "'" in
		let id_pp = Uniq.pp (uniq_of a) in
		PP.(!^ vt_str + !^ "z" + id_pp)
	and pp_fun = fun {domain; effects; range; varargs} ->
		let args_pp = pp_args domain varargs in
		let res_pp = pp range in
		let eff_pp = EffectVar.pp_lb effects in
		let arr_pp = PP.(!^ "==" + eff_pp + !^ "==>") in
		PP.(parens(args_pp ++ arr_pp ++ res_pp))
	(* THINK: Should I turn it into PP.tuple ~pp ? *)
	and pp_args args varargs =
		let pp_varargs =
			if varargs then PP.(comma ++ !^ "...") else PP.empty
		in
		PP.(parens (comma_sep (List.map pp args) + pp_varargs))

	let to_string = PP.to_string % pp

    end

(* Memory regions *)

and Region : sig

	type t

	val uniq_of : t -> Uniq.t

	val is_meta : t -> bool

	val compare : t -> t -> int

	val bound : unit -> t

	val meta : unit -> t

	val write : t -> t -> unit

	val zonk : t -> t

	val (=~) : t -> t -> unit

	val vsubst : Var.t Subst.t -> t -> t

	val pp : t -> PP.doc

	val to_string : t -> string

end = struct

	type t = Bound of Uniq.t
		   | Meta of Uniq.t * t Meta.t

	let uniq_of = function
		| Bound u
		| Meta(u,_)
			-> u

	let is_meta = function
		| Meta _  -> true
		| Bound _ -> false

	let compare = Utils.compare_on uniq_of

	let equals r1 r2 = compare r1 r2 = 0

	let bound () : Region.t =
		let id = Uniq.fresh() in
		Bound id

	let meta () : Region.t =
		let id = Uniq.fresh() in
		let mr = Meta.fresh() in
		Meta(id,mr)

	(* THINK: just use =~ ? *)
	let write r1 r2 =
		match r1 with
		| Bound _    -> Error.panic_with("write: not a meta-region")
		| Meta(_,mr) -> Meta.write mr r2

	let rec zonk :Region.t -> Region.t = function
		| Bound _ as r ->
			r
		| Meta(id,mr) as r ->
			Option.(Meta.zonk_with zonk mr |? r)

	let unify_unbound id1 mr1 = function
		| Meta(id2,_) when id1 = id2 -> ()
		| Bound _ -> Error.panic()
		| r2 -> Meta.write mr1 r2

	(* TODO: Refactor code in Meta *)
	let rec (=~) r1 r2 :unit =
		if equals r1 r2
		then ()
		else
		match r1 with
		| Meta(id1,mr1) ->
		   (match Meta.read mr1 with
		   | None ->
			   let r2' = zonk r2 in
			   unify_unbound id1 mr1 r2'
		   | Some rr1 -> rr1 =~ r2
		   )
		| ___ -> Error.panic()

	let vsubst s r =
		let x = Var.Region r in
		Var.to_region (Subst.find_default x x s)

	(* THINK: Should I refactor this into PP.tyvar ? *)
	let pp r =
		let vt_str = if is_meta r then "?" else "'" in
		let id_str = Uniq.to_string (uniq_of r) in
		PP.(!^ (Utils.green (vt_str ^ "r" ^ id_str)))

	let to_string = PP.to_string % pp

end

and Regions : sig
	include Set.S with type elt := Region.t
	val none : t
	val (+) : t -> t -> t
	val (-) : t -> t -> t
	val sum : t list -> t
	val pp : t -> PP.doc
	val to_string : t -> string
	end
	= struct
		include Set.Make(Region)
		let none = empty
		let (+) = union
		let (-) = diff
		let sum = List.fold_left union none
		let pp x = PP.braces (PP.space_sep (List.map Region.pp (elements x)))
		let to_string = PP.to_string % pp
	end

and EffectVar : sig

	type lb = Effects.t

	type t

	val lb_of : t -> lb

	val uniq_of : t -> Uniq.t

	val compare : t -> t -> int

	val bound_with : lb -> t

	val is_meta : t -> bool

	val meta_with : lb -> t

	val meta : unit -> t

	val fv_of : t -> Vars.t

	val add_lb : Effects.t -> t -> t

	val write : t -> t -> unit

	val zonk : t -> t

	val (=~) : t -> t -> unit

	val vsubst : Var.t Subst.t -> t -> t

	val pp : t -> PP.doc

	val pp_lb : t -> PP.doc

	val to_string : t -> string

end = struct

	type lb = Effects.t

	type meta_lb = lb Uref.t

	type meta_uniq = Uniq.t Uref.t

	type t = Bound of Uniq.t * lb
		   | Meta of meta_eff

	and meta_eff = meta_info ref

	and meta_info = Root of Uniq.t * lb
	              | Link of Uniq.t * t

	let uniq_of_meta mf =
		match !mf with
		| Root(u,_)
		| Link(u,_) -> u

	let uniq_of = function
		| Bound(u,_) -> u
		| Meta mf    -> uniq_of_meta mf

	let bound_with lb =
		let id = Uniq.fresh() in
		Bound(id,lb)

	let is_meta = function
		| Bound _ -> false
		| Meta _  -> true

	let get_meta = function
		| Bound _ -> Error.panic_with("get_meta: not a meta-effect variable")
		| Meta mf -> mf

	let rec lb_of = function
		| Bound(_,lb) -> lb
		| Meta mf     -> lb_of_meta mf

	and lb_of_meta mf =
		match !mf with
		| Root(_,lb) -> lb
		| Link(_,ff) -> lb_of ff

	let compare = Utils.compare_on uniq_of

	let equals f1 f2 = compare f1 f2 = 0

	let fv_of = Effects.fv_of % lb_of

	let meta_with lb :t =
		let id = Uniq.fresh() in
		let mf = ref (Root(id,lb)) in
		Meta mf

	let meta () =
		meta_with Effects.none

	(* Map with path-compression. *)
	let rec map (fn :lb -> lb) :t -> t = function
		| Bound(id,lb) ->
			Bound(id,fn lb)
		| Meta mf ->
			map_meta fn mf

	and map_meta fn mf =
		match !mf with
		| Root(id,lb) ->
			let mi' = Root(id,fn lb) in
			mf := mi';
			Meta mf
		| Link(id,ff) ->
			let ff' = map fn ff in
			mf := Link(id,ff'); (* path compression *)
			ff'

	let zonk = map Effects.zonk

	let rec add_lb delta =
		map (fun lb -> Effects.(delta + lb))

	let unify_unbound u1 lb1 mf1 f2 =
		if uniq_of f2 = u1
		then ()
		else mf1 := Link(u1,add_lb lb1 f2)

	let rec (=~) f1 f2 :unit =
		assert (is_meta f1);
		assert (is_meta f2);
		if equals f1 f2
		then ()
		else
			unify_meta (get_meta f1) (zonk f2)

	and unify_meta mf1 f2 =
		match !mf1 with
		| Link(_,ff) -> ff =~ f2
		| Root(u,lb) -> unify_unbound u lb mf1 f2

	let write f1 f2 =
		assert (is_meta f1);
		let u = uniq_of f1 in
		let mf = get_meta f1 in
		mf := Link(u,f2)

	let vsubst_lb s = map (Effects.vsubst s)

	let vsubst s f =
		let x = Var.Effect f in
		let f' = Var.to_effect (Subst.find_default x x s) in
		vsubst_lb s f'

	let pp f =
		let vt_str = if is_meta f then "?" else "'" in
		let id_pp = Uniq.pp (uniq_of f) in
		PP.(!^ vt_str + !^ "f" + id_pp)

	let pp_lb f =
		let lb = lb_of f in
		PP.(pp f + !^ "@" + Effects.pp lb)

	let to_string = PP.to_string % pp

end

(* Effects *)

and Effects : sig

	type mem_kind = Alloc | Free | Read | Write | Uninit | Call | Lock | Unlock

	type e = Var of EffectVar.t
		   | Mem of mem_kind * Region.t
		   | Noret
		   | IrqsOn
		   | IrqsOff
		   | BhsOn
		   | BhsOff

	type 'a certainty = private Must of 'a | May of 'a

	val may : 'a -> 'a certainty

	val must : 'a -> 'a certainty

	val is_may : 'a certainty -> 'a option

	val is_must : 'a certainty -> 'a option

	(** Weaken the certainty or, in other words, sets it to [may]. *)
	val weak : 'a certainty -> 'a certainty

	val uncertain : 'a certainty -> 'a

	module EffectSet : Set.S with type elt := e

	type t = {
		  must : EffectSet.t
		; may  : EffectSet.t
	}

	val (=.) : e -> e -> bool

	val (=~) : e certainty -> e certainty -> bool

	val none : t

	val allocs : r:Region.t -> e

	val frees : r:Region.t -> e

	val reads : r:Region.t -> e

	val writes : r:Region.t -> e

	val uninits : r:Region.t -> e

	val calls : r:Region.t -> e

	val locks : r:Region.t -> e

	val unlocks : r:Region.t -> e

	val noret : e

	val is_reads : e -> bool

	val is_writes : e -> bool

	val is_uninit : e -> bool

	val is_locks : e -> bool

	val is_uninits : e -> bool

	val just_var : EffectVar.t -> t

	val just : e -> t

	(** Read all memory regions of a given shape. *)
	val fully_read : Shape.t -> t

	(** Weaken the certaint of the effects. *)
	val weaken : t -> t

	val mem : e -> t -> bool

	val mem_must : e -> t -> bool

	val (+.) : t -> e -> t

	val (+) : t -> t -> t

	val filter : (e -> bool) -> t -> t

	val remove : e -> t -> t

	val compare : t -> t -> int

	val fv_of : t -> Vars.t

	val regions : t -> Regions.t

	val enum_regions : t -> Region.t Enum.t

	val vsubst : Var.t Subst.t -> t -> t

	val of_enum : e certainty Enum.t -> t

	val enum : t -> e certainty Enum.t

	val enum_may : t -> e Enum.t

	val enum_principal : t -> e certainty Enum.t

	val principal : t -> t

	val zonk : t -> t

	val pp : t -> PP.doc

	val to_string : t -> string

	end
	= struct

	type mem_kind = Alloc | Free | Read | Write | Uninit | Call | Lock | Unlock

	type e = Var of EffectVar.t
		   | Mem of mem_kind * Region.t
		   | Noret
		   | IrqsOn
		   | IrqsOff
		   | BhsOn
		   | BhsOff

	type 'a certainty = Must of 'a | May of 'a

	let may x = May x

	let must x = Must x

	let is_may = function
		| May x  -> Some x
		| Must _ -> None

	let is_must = function
		| Must x -> Some x
		| May _  -> None

	let weak = function
		| Must f -> May f
		| x      -> x

	let uncertain = function
		| Must f -> f
		| May  f -> f

	let mk_var x =
		Var x

	let mk_mem ~k ~r =
		Mem(k,r)

	let compare_e f1 f2 =
		match (f1,f2) with
		| (Var x,Var y) -> EffectVar.compare x y
		| (Mem (k1,r1),Mem (k2,r2)) ->
			let cmp_k = Pervasives.compare k1 k2 in
			if cmp_k = 0
			then Region.compare r1 r2
			else cmp_k
		| _other -> Pervasives.compare f1 f2

	let (=.) e1 e2 = compare_e e1 e2 = 0

	let (=~) f1 f2 = match (f1,f2) with
		| (Must x,Must y) -> x =. y
		| (May x,May y)   -> x =. y
		| __otherwise__   -> false

	module EffectSet = Set.Make(
		struct
			type t = e
			let compare = compare_e
		end
		)

	(* THINK: possible optimization is to assume must implicitly contained in may, to keep may smaller *)
	type t = {
		  must : EffectSet.t
		; may  : EffectSet.t (* must is a subset of may *)
	}

	let none = {
		  may  = EffectSet.empty
		; must = EffectSet.empty
	}

	let just e = {
		  may  = EffectSet.singleton e
		; must = EffectSet.singleton e
	}

	let allocs ~r = mk_mem Alloc r

	let frees ~r = mk_mem Free r

	let reads ~r = mk_mem Read r

	let writes ~r = mk_mem Write r

	let uninits ~r = mk_mem Uninit r

	let calls ~r = mk_mem Call r

	let locks ~r = mk_mem Lock r

	let unlocks ~r = mk_mem Unlock r

	let noret = Noret

	let is_mem k = function
		| Mem(k1,_) -> k = k1
		| __other__ -> false

	let is_reads = is_mem Read

	let is_writes = is_mem Write

	let is_uninit = is_mem Uninit

	let is_locks = is_mem Lock

	let is_uninits = is_mem Uninit

	let just_var x = just (mk_var x)

	let weaken fs = { fs with must = EffectSet.empty }

	let add f fs = {
		  may  = EffectSet.add f fs.may
		; must = EffectSet.add f fs.must
		}

	let mem f fs =
		EffectSet.mem f fs.may

	let mem_must f fs =
		EffectSet.mem f fs.must

	let (+.) fs f = add f fs

	let (+) fs1 fs2 =
		{ may  = EffectSet.union fs1.may fs2.may
		; must = EffectSet.union fs1.must fs2.must
		}

	let fully_read z =
		let open Shape in
		let f = {
			var = const none ;
			bot = none ;
			ptr = identity ;
			rf  = (fun r v -> v +. reads r) ;
			fn  = const none
		}
		in
		fold f z

	let filter pred fs = {
		  may  = EffectSet.filter pred fs.may
		; must = EffectSet.filter pred fs.must
	}

	let remove f fs = {
		  may  = EffectSet.remove f fs.may
		; must = EffectSet.remove f fs.must
	}

	let compare fs1 fs2 =
		(* better to compare the smaller set first *)
		let cmp_must = EffectSet.compare fs1.must fs2.must in
		if cmp_must = 0
		then EffectSet.compare fs1.may fs2.may
		else cmp_must

	let of_enum fe =
		let (mayE,mustE) = Enum.span (Option.is_some % is_may) fe in
		let mays = mayE |> Enum.map uncertain |> EffectSet.of_enum in
		let musts = mustE |> Enum.map uncertain |> EffectSet.of_enum in
		assert(EffectSet.subset musts mays);
		{ may  = mays
		; must = musts
		}

	let enum fs =
		let mays  = fs.may  |> EffectSet.enum |> Enum.map may  in
		let musts = fs.must |> EffectSet.enum |> Enum.map must in
		Enum.append mays musts

	let enum_may fs = EffectSet.enum fs.may

	let fv_of fs =
		let ff = fs |> enum_may |> List.of_enum in
		let xss = ff |> List.map (function
			| Var x    -> EffectVar.fv_of x
			| Mem(_,r) -> Vars.singleton (Var.Region r)
			| _other   -> Vars.none
		) in
		Vars.sum xss

	(* TODO: Use lazy lists instead ? *)
	let enum_regions ef =
		let get_region = function
			| Mem(_,r) -> Some r
			| ________ -> None
		in
		ef |> enum_may |> Enum.filter_map get_region

	let regions = Regions.of_enum % enum_regions

	let vsubst_e (s :Var.t Subst.t) :e -> e
		= function
		| Var x     -> Var (EffectVar.vsubst s x)
		| Mem (k,r) -> Mem (k,Region.vsubst s r)
		| e         -> e

	let vsubst (s :Var.t Subst.t) fs :t = {
		  may  = EffectSet.map (vsubst_e s) fs.may
		; must = EffectSet.map (vsubst_e s) fs.must
	}

	let rec enum_principal f =
		Enum.concat (Enum.map principal_of_e (enum f))

	and principal_of_e (f :e certainty) :e certainty Enum.t =
		match f with
		| May(Var x) ->
			let en = enum_principal (EffectVar.lb_of x) in
			let en' = Enum.map weak en in
			Enum.push en' f;
			en'
		| Must(Var x) ->
			let en = enum_principal (EffectVar.lb_of x) in
			Enum.push en f;
			en
		| _____ -> Enum.singleton f

	let principal f = of_enum (enum_principal f)

	let zonk_e = function
		| Var f    -> Var (EffectVar.zonk f)
		| Mem(k,r) -> let r' = Region.zonk r in
					  Mem(k,r')
		| e        -> e

	let zonk fs = {
		  may  = EffectSet.map zonk_e fs.may
		; must = EffectSet.map zonk_e fs.must
	}

	let string_of_kind = function
		| Read   -> "read"
		| Write  -> "write"
		| Uninit -> "uninit"
		| Call   -> "call"
		| Alloc  -> "alloc"
		| Free   -> "free"
		| Lock   -> "lock"
		| Unlock -> "unlock"

	let pp_kind k = PP.(!^) (Utils.purple (string_of_kind k))

	let pp_e = function
		| Var x    -> EffectVar.pp x
		| Mem(k,r) -> PP.(pp_kind k + brackets(Region.pp r))
		| Noret    -> PP.(!^ (Utils.purple "noret"))
		| IrqsOn   -> PP.(!^ (Utils.purple "irqson"))
		| IrqsOff  -> PP.(!^ (Utils.purple "irqsoff"))
		| BhsOn    -> PP.(!^ (Utils.purple "bhson"))
		| BhsOff   -> PP.(!^ (Utils.purple "bhsoff"))

	let group_effects : e list -> e list list =
		let same_kind e1 e2 =
			match (e1,e2) with
			| (Mem(k1,_),Mem(k2,_)) -> Pervasives.compare k1 k2
			| _other                -> compare_e e1 e2
		in
		List.group same_kind

	let pp_group : e list -> PP.doc = function
		| (Mem(k,_)::_) as es ->
			let pp_rs = List.map (function
				| (Mem(_,r)) -> Region.pp r
				| _other____ -> Error.panic()
			) es
			in
			PP.(pp_kind k + brackets(PP.comma_sep pp_rs))
		| [e]                 -> pp_e e
		| _other_____________ -> Error.panic()

	let pp fs =
		let open EffectSet in
		let grouped = group_effects % to_list in
		let strictly_may = diff fs.may fs.must in
		let pp_mays = List.map pp_group (grouped strictly_may) in
		let pp_must = List.map (PP.prefix "!" % pp_group) (grouped fs.must) in
		PP.braces(PP.space_sep (pp_must @ pp_mays))

	let to_string :t -> string = PP.to_string % pp

	end

(* THINK: Should I turn it into a functor? *)
and Meta : sig

	type 'a t

	val fresh : unit -> 'a t

	val read : 'a t -> 'a option

	val write : 'a t -> 'a -> unit

	val map_default : ('a -> 'b) -> 'b -> 'a t -> 'b

	val fv_with : fv_of:('a -> Vars.t) -> 'a t -> Vars.t

	val zonk_with : ('a -> 'a) -> 'a t -> 'a option

end = struct

	type 'a t = 'a option ref

	let fresh () = ref None

	let read mx = !mx

	let write mx x = mx := Some x

	let map_default f b mx =
		Option.map_default f b !mx

	let fv_with ~fv_of = map_default fv_of Vars.none

	let zonk_with zonk mx =
		match read mx with
		| None   -> None
		| Some x ->
			let x' = zonk x in
			write mx x';
			Some x'

end

(* Variables *)

and Var : sig

	(* NB: Effect subeffecting constraints should not be cyclic or
	   the FV computation will loop...
	   What about recursive functions???
	 *)

	type t = Shape  of Shape.var
	       | Effect of EffectVar.t
	       | Region of Region.t


	and kind = Shp | Eff | Reg

	val kind_of : t -> kind

	val is_effect : t -> bool

	val is_region : t -> bool

	val is_shape : t -> bool

	val to_shape : t -> Shape.var

	val to_effect : t -> EffectVar.t

	val to_region : t -> Region.t

	val uniq_of : t -> Uniq.t

	val compare : t -> t -> int

	val meta_of : t -> t

	val meta_of_list : t list -> t list

	val bound_of_list : t list -> t list

	val write : t -> t -> unit

	val zonk_lb : t -> t

	val pp : t -> PP.doc

	val to_string : t -> string

	end
	= struct

	type t = Shape  of Shape.var
	       | Effect of EffectVar.t
	       | Region of Region.t


	and kind = Shp | Eff | Reg

	let kind_of = function
		| Shape _  -> Shp
		| Effect _ -> Eff
		| Region _ -> Reg

	let is_shape x = kind_of x = Shp

	let is_effect x = kind_of x = Eff

	let is_region x = kind_of x = Reg

	let to_shape = function
		| Shape a -> a
		| _______ -> Error.panic()

	let to_effect = function
		| Effect f -> f
		| ________ -> Error.panic()

	let to_region = function
		| Region r -> r
		| ________ -> Error.panic()

	let is_meta = function
		| Shape a  -> Shape.is_meta a
		| Effect f -> EffectVar.is_meta f
		| Region r -> Region.is_meta r

	let uniq_of = function
		| Shape a  -> Shape.uniq_of a
		| Effect f -> EffectVar.uniq_of f
		| Region r -> Region.uniq_of r

	let compare x y = Pervasives.compare (uniq_of x) (uniq_of y)

	let bound_of :t -> t = function
		| Shape _  -> Shape(Shape.bound_var())
		| Region _ -> Region(Region.bound())
		| Effect f ->
			let lb = EffectVar.lb_of f in
			Effect(EffectVar.bound_with lb)

	let bound_of_list :t list -> t list =
    	List.map bound_of

	let meta_of :t -> t = function
		| Shape _  -> Shape(Shape.meta_var())
		| Region _ -> Region(Region.meta())
		| Effect f ->
			let lb = EffectVar.lb_of f in
			Effect(EffectVar.meta_with lb)

	let meta_of_list :t list -> t list =
    	List.map meta_of

	let write x y =
		assert (is_meta x);
		match (x,y) with
		| (Shape a,Shape b)   -> Shape.write_var a (Shape.Var b)
		| (Effect f,Effect g) -> EffectVar.write f g
		| (Region r,Region s) -> Region.write r s
		| __other__           -> Error.panic_with("write: incompatible types")

	(* TODO: refactor into a Var.map function *)
	let zonk_lb = function
		| Effect f -> Effect(EffectVar.zonk f)
		| x -> x

	let pp = function
		| Shape a  -> Shape.pp_var a
		| Effect f -> EffectVar.pp f
		| Region r -> Region.pp r

	let to_string = PP.to_string % pp

	end

and Vars : sig
	include Set.S with type elt := Var.t
	val none : t
	val (+) : t -> t -> t
	val sum : t list -> t
	val zonk_lb : t -> t
	val pp : t -> PP.doc
	val to_string : t -> string
	end
	= struct
		include Set.Make(Var)
		let none = empty
		let (+) = union
		let sum = List.fold_left union none
		let zonk_lb = map Var.zonk_lb
		let pp x = PP.braces (PP.space_sep (List.map Var.pp (elements x)))
		let to_string = PP.to_string % pp
	end

and VarMap : sig
	include Map.S with type key := Var.t
	end
	= Map.Make(Var)

and Subst : sig

	type 'a t

	val mk : (Var.t * 'a) list -> 'a t

	val find : Var.t -> 'a t -> 'a option

	val find_default : 'a -> Var.t -> 'a t -> 'a

	end
	= struct

	module VarMap = Map.Make(Var)

	type 'a t = 'a VarMap.t

	let mk : (Var.t * 'a) list -> 'a t = fun xs ->
		(* assert all variables are "Bound" *)
		VarMap.of_enum (List.enum xs)

	let find = VarMap.Exceptionless.find

	let find_default v x s =
		Option.(find x s |? v)

	end

(* Subeffecting Constraints *)

and Constraints : sig
	type t = Vars.t
	val none : t
	val add : EffectVar.t -> Effects.t -> t -> t
	val (+) : t -> t -> t
	val minus : t -> t -> t
	val cardinal : t -> int
	end =
	struct

	type elt = EffectVar.t

    type t = Vars.t

    let none :t = Vars.none

    let add x f k =
		let _ = EffectVar.add_lb (Effects.remove (Effects.Var x) f) x in
		Vars.add (Var.Effect x) k

    let (+) = Vars.(+)

    let minus = Vars.diff

	let cardinal = Vars.cardinal

	end

and Scheme : sig
	type 'a t = { vars : Vars.t; body : 'a }
	val instantiate : Shape.t t -> Shape.t * Constraints.t
	val quantify : Vars.t -> Shape.t -> Shape.t t
	val ref_of : Region.t -> Shape.t t -> Shape.t t
	val regions_in : Shape.t t -> Regions.t
	val of_shape : Shape.t -> Shape.t t
	val of_varinfo : Cil.varinfo -> Shape.t t
	(* TODO: Should return Effects.t *)
	val effects_of_fun : Shape.t t -> Effects.e Effects.certainty Enum.t
	val fresh_binding : Cil.varinfo -> Cil.varinfo * Shape.t t
	val fresh_bindings : Cil.varinfo list -> (Cil.varinfo * Shape.t t) list
	val fv_of : Shape.t t -> Vars.t
	val zonk : Shape.t t -> Shape.t t
	val pp : Shape.t t -> PP.doc
	val to_string : Shape.t t -> string
end = struct

	type 'a t = { vars : Vars.t; body : 'a }

	let instantiate :Shape.t t -> Shape.t * Constraints.t =
		fun { vars; body = shp} ->
			let qvs = Vars.to_list vars in
			let mtvs = Var.meta_of_list qvs in
			let s = Subst.mk (List.combine qvs mtvs) in
			let shp' = Shape.vsubst s shp in
			let k = Vars.filter Var.is_effect (Shape.fv_of shp') in
			shp', k

	(* THINK: [let xs = Vars.map f vs] to avoid intermediate lists *)
	let quantify vs z : Shape.t t =
		let ys = Vars.to_list vs in
		let xs = Var.bound_of_list ys in
		(* write into vs's *)
		List.iter2 Var.write ys xs;
		let xs' = Vars.zonk_lb (Vars.of_list xs) in
		let z' = Shape.zonk z in
		{ vars = xs'; body = z' }

	let ref_of r sch =
		{ sch with body = Shape.Ref(r,sch.body) }

	let regions_in sch = Shape.regions_in sch.body
	let of_shape z = { vars = Vars.none; body = z }
	let of_varinfo x = of_shape (Shape.of_varinfo x)
	let effects_of_fun sch =
		let _,f,_ = Shape.get_ref_fun sch.body in
		Effects.enum_principal (EffectVar.lb_of f)
	let fresh_binding x = x, of_varinfo x
	let fresh_bindings = List.map fresh_binding
	let fv_of sch = Shape.fv_of sch.body
	(* THINK: also Vars.zonk_lb ? *)
	let zonk sch = { sch with body = Shape.zonk sch.body }

	let pp {vars; body} =
		PP.(!^ "forall" ++ Vars.pp vars + !^ "." ++ Shape.pp body)

	let to_string = PP.to_string % pp
end

module E = Effects

module type FVable = sig
	type t
	val fv_of : t -> Vars.t
end

module K = Constraints

type shape = Shape.t
type effects = Effects.t
type var = Var.t
type region = Region.t
type 'a scheme = 'a Scheme.t


(* Unification *)

module Unify =
	struct

	open Shape

	(* In this application, these should be a panic().
	 * We assume the program type-checks.
	 *)
	exception Cannot_unify of shape * shape
	exception Occurs_check of Shape.var * shape

	let ok = ()

	let fail_cannot_unify s1 s2 = raise (Cannot_unify (s1,s2))

	let rec (=~) s1 s2 =
		match (s1,s2) with
		| (Var a,Var b) when a = b ->
			assert (Shape.is_meta a);
			assert (Shape.is_meta b);
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
		-> Region.(r =~ s);
		   x =~ y
		| __otherwise__
		-> fail_cannot_unify s1 s2

    and unify_fun f1 f2 =
    	let unify_dom d1 d2 =
    		try List.iter2 (=~) d1 d2
    		with Invalid_argument _ ->
				(* Oops unsound. Doing anything more complex than this is probably not worth
				 * the effort. Passing more arguments than required is already supported anyways.
				 *)
				Log.error "Couldn't unify function types with different number of arguments %s ~ %s"
					(PP.to_string (Shape.pp_fun f1)) (PP.to_string (Shape.pp_fun f2));
				()
    	in
    	let { domain = dom1; effects = ef1; range = res1 } = f1 in
    	let { domain = dom2; effects = ef2; range = res2 } = f2 in
    	unify_dom dom1 dom2;
    	EffectVar.(ef1 =~ ef2);
    	res1 =~ res2

	and unify_var a z =
		assert (Shape.is_meta a);
		match Shape.read_var a with
		| None -> let z' = Shape.zonk z in
		          unify_unbound_var a z'
		| Some z1 -> z1 =~ z

	and unify_unbound_var a = function
		| Var b when a = b ->
			ok
		| z when Shape.free_in (Var.Shape a) z ->
			Log.warn "Cyclic shape: %s ~ %s" Shape.(to_string (Var a)) (Shape.to_string z);
			ok (* BUT UNSOUND: raise(Occurs_check(a,z)) *)
		| z ->
			Shape.write_var a z

	(* z =~ ptr z1 *)
	let match_ptr_shape z : shape =
		match z with
		| Ptr z1 -> z1
		| Var a  -> let z1 = Shape.fresh() in
					unify_var a (Ptr z1);
					z1
		| ______ -> Error.panic()

	(* z =~ ref (r,z1) *)
	let match_ref_shape (z :shape) :Region.t * shape =
		match z with
		| Ref (r,z1) -> r, z1
		| Var a      ->
			let z1 = Shape.fresh() in
			let r = Region.meta() in
			unify_var a (Ref (r,z1));
			r, z1
		| __________ ->
			Error.panic_with(Printf.sprintf "Not a ref shape: %s" (Shape.to_string z))

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
