open Batteries
open Type

open Shape

(* Environment *)

module Env =
	struct

	(* THINK: Global and local env, for the global one we can precompute fv_of
	   since it will be needed quite often. *)

    module IntMap = Map.Make(Int)

	type type_scheme = shape scheme

	(** A map from CIL uniques to shape schemes *)
	type t = type_scheme IntMap.t

	let empty = IntMap.empty

	let add (x :Cil.varinfo) (ty :type_scheme) :t -> t =
		IntMap.add Cil.(x.vid) ty

	let singleton x ty = add x ty empty

	let (+::) : Cil.varinfo * type_scheme -> t -> t =
		fun (x,ty) -> add x ty

	(* Addition of disjoint environments *)
	let (+>) (env1 :t) (env2 :t) :t =
		(* assert (disjoint env1 env2 *)
		IntMap.fold
			(fun i ty env -> IntMap.add i ty env)
			env1
			env2

	let find (x:Cil.varinfo) (env :t) :type_scheme =
		IntMap.find Cil.(x.vid) env

	let of_list :(Cil.varinfo * type_scheme) list -> t =
		IntMap.of_enum % Enum.map (fun (x,y) -> Cil.(x.vid),y) % List.enum

	let of_cil_var (x :Cil.varinfo) :t =
		let ty = Cil.(x.vtype) in
		let z = Shape.ref_of ty in
		singleton x { vars = Vars.empty; body = z }

	let of_cil_vars :Cil.varinfo list -> t =
		List.fold_left (fun env x -> of_cil_var x +> env) empty

	(* TODO: return env' and env'', env' for observe ? *)
	let of_fundec (env :t) (fd :Cil.fundec) :shape * t =
		let fn = Cil.(fd.svar) in
		let args = Cil.(fd.sformals) in
		let shp' = Shape.of_typ Cil.(fd.svar.vtype) in
		let args_shp',_,_  = Shape.get_fun shp' in
		let sch' = { vars = Vars.empty; body = shp' } in
		let args_sch' = List.map (fun z ->
							{ vars = Vars.empty; body = z })
							args_shp'
		in
		let args_env = of_list (List.combine args args_sch') in
		let env' = (fn,sch') +:: (args_env +> env) in
		shp', env'

	let of_fundec_locals (env :t) (fd :Cil.fundec) :t =
		of_cil_vars Cil.(fd.slocals) +> env

	let fv_of (env :t) :Vars.t =
		(* FIXME: This is ignoring the constraints... *)
		IntMap.fold (fun _ {body = shp} ->
			Vars.union (Shape.fv_of shp)
			) env Vars.empty

	end

(* NOTE: We need to keep track of subeffecting constraints almost everywhere.
   When we instantiate a variable we get subeffecting constraints, that must
   be propagated bottom-up, until we reach a function definition.
   However, constraints are not necessary to type-check expressions,
   so we don't push them into subterms.
 *)

(* NOTE: Variable declarations cannot be generalized:
		int x = 1
	equals to
		let x = ref 1
	this would not be sound, we also impose the value restriction.
 *)

let instantiate :shape scheme -> shape * K.t =
	fun { vars; body = shp} ->
	let qvs = Vars.to_list vars in
	let mtvs = Var.of_bounds qvs in
	let s = Subst.mk (List.combine qvs mtvs) in
	let shp' = Shape.vsubst s shp in
	let k = Vars.filter Var.is_effect (fv_of shp') in
	shp', k

(* If we generalize meta-type variables we could just write the meta variables and then zonk, instead of substitution. *)
let quantify (vs :Vars.t) (z :shape)
	: shape scheme =
	let ys = Vars.to_list vs in
	let xs = List.map (Var.fresh % Var.kind_of) ys in
	let s = Subst.mk (List.combine ys xs) in
	let z' = Shape.vsubst s z in
	{ vars = Vars.of_list xs; body = z' }

(* generalize effect-free shapes, such as functions *)
let generalize (env :Env.t) (k :K.t) (z :shape)
	: shape scheme * K.t =
	let zz = Shape.zonk_shape z in
	let z_fv = Shape.fv_of zz in
	let env_fv = Env.fv_of env in
	let vs = Vars.diff z_fv env_fv in
	let k1 = K.minus k vs in
	let sch = quantify vs z in
	sch, k1

let rec principal_effects f =
	Enum.concat (Enum.map principal_effects_e (Effects.enum f))

and principal_effects_e (f :Effects.e) :Effects.e Enum.t =
	match f with
	| Effects.Var x -> 
		let en = principal_effects (Uref.uget (Var.lb_of x)) in
		Enum.push en f;
		en
	| _____________ -> Enum.singleton f

let observe (env :Env.t) :E.t -> E.t =
	let env_fv = Env.fv_of env in
    let is_observable = function
		| E.Var x     -> Vars.mem x env_fv
		| E.Mem(_k,r) -> Vars.mem r env_fv
	in
	E.of_enum % Enum.filter is_observable % principal_effects

(* TODO: We should keep "precision" info associated with
   region variables. E.g. to know if something is the result
   of unsafe pointer arithmetic.
 *)

let of_const : Cil.constant -> shape
	= function
	| Cil.CInt64 _ -> Shape.new_shape()
	| Cil.CStr _   -> Bot
	| Cil.CWStr _  -> Bot
	| Cil.CChr _   -> Bot
	| Cil.CReal _  -> Bot
	| Cil.CEnum _  -> Bot

(* The three unary operators -, ~, and ! do not affect the shape
	   of the expression *)
let of_unop (_env :Env.t) z
	: Cil.unop -> shape * Effects.t
	= fun _ -> z, Effects.none

(* For now, we just pick the shape of the first argument, which in CIL,
   it's always the pointer argument. This may be unsound in some corner
   (rare?) cases, but we are unsound anyways. Better to be precise in the
   majority of cases.

   TODO: PlusPI -> mark that a pointer is the result of arithmetic
		 MinusPP -> warn if after unification the two regions are different
 *)
let of_binop (_env :Env.t) z1 _z2
	: Cil.binop -> shape * Effects.t
	= fun _ -> z1, Effects.none

let rec of_exp (env :Env.t)
	: Cil.exp -> shape * Effects.t * K.t
	= function
	| Cil.Const c
	-> of_const c, Effects.none, K.none
	| Cil.Lval lv
	-> let z, f, k = of_lval env lv in
	   let r, z0 = Unify.match_ref_shape z in
	   let f' = E.(f + read r) in
	   z0, f', k
	(* Even though effectively [unsigned int] or the like,
	 * it seems a terrible idea to cast [size_t] to a
	 * pointer type, so we give it shape _|_.
	 *)
	| Cil.SizeOf _
	| Cil.SizeOfE _
	| Cil.SizeOfStr _
	| Cil.AlignOf _
	| Cil.AlignOfE _
	-> Bot, Effects.none, K.none
	(* These operators may add effect, but they don't hide
	   any of the effects of their arguments *)
	| Cil.UnOp (op,e,_ty)
	-> let z, f, k = of_exp env e in
	   let z1, f1 = of_unop env z op in
	   z1, Effects.(f + f1), k
	| Cil.BinOp (op,e1,e2,ty)
	-> let z1, f1, k1 = of_exp env e1 in
	   let z2, f2, k2 = of_exp env e2 in
	   let z3, f3 = of_binop env z1 z2 op in
	   z3, Effects.(f1 + f2 + f3), Constraints.(k1 + k2)
	| Cil.Question (e1,e2,e3,ty) ->
	  let _z1, f1, k1 = of_exp env e1 in
	  let  z2, f2, k2 = of_exp env e2 in
	  let  z3, f3, k3 = of_exp env e3 in
	  Unify.(z2 =~ z3);
	  z2, Effects.(f1 + f2 + f3), Constraints.(k1 + k2 + k3)
	| Cil.CastE (ty,e)
	-> let z, f, k = of_exp env e in
	   let z1 = Unify.match_shape_with_typ z ty in
	   z1, f, k
	| Cil.AddrOf lv
	-> let z, f, k = of_lval env lv in
	   Ptr z, f, k
	(* TODO: This is a GCC extension, I hope not a popular one :-) *)
	| Cil.AddrOfLabel _  -> Error.not_implemented()
	(* startof is a CIL operator to represent the implicit cast
	   between an array variable to a pointer to the first element,
	   effectively [id] for us.
	 *)
	| Cil.StartOf lv
	-> of_lval env lv

and of_lval (env :Env.t)
	: Cil.lval -> shape * Effects.t * Constraints.t
	= function (lhost,offset) ->
		let z, f, k = of_lhost env lhost in
		let z1, f1, k1 = with_offset env z offset in
		z1, Effects.(f + f1), Constraints.(k + k1)

and with_offset (env: Env.t) (z :shape)
	: Cil.offset -> shape * Effects.t * Constraints.t
	= function
	| Cil.NoOffset      -> z, Effects.none, Constraints.none
	(* array indexing *)
	| Cil.Index (e,off) ->
		let _z0, f0, k0 = of_exp env e in
		(* z = ref z1 *)
		let r, z1 = Unify.match_ref_shape z in
		(* z = ref ptr z2 *)
		let z2 = Unify.match_ptr_shape z1 in
		z2, Effects.(f0 + read r), k0
	(* record field *)
	| Cil.Field _ -> Error.not_implemented()

and of_lhost (env :Env.t)
	: Cil.lhost -> shape * Effects.t * Constraints.t
	= function
	| Cil.Var x ->
		let sch = Env.find x env in
		let z, k = instantiate sch in
		(* assert (is_ref_shape z) ? *)
		z, Effects.none, k
	| Cil.Mem e ->
		let z, f, k = of_exp env e in
		let z1 = Unify.match_ptr_shape z in
		z1, f, k

let with_lval_set (env :Env.t) z f k lv : Effects.t * Constraints.t =
	let z1, f1, k1 = of_lval env lv in
	let r, z0 = Unify.match_ref_shape z1 in
	Unify.(z0 =~ z);
	Effects.(f + f1 + write r), Constraints.(k + k1)

let of_instr (env :Env.t)
	: Cil.instr -> Effects.t * Constraints.t
	= function
	| Cil.Set (lv,e,_loc)
	-> let z, f, k = of_exp env e in
	   with_lval_set env z f k lv
	| Cil.Call (lv_opt,fn,es,_loc)
	-> (* z' fn(zs) | f *)
	   let z0, f0, k0 = of_exp env fn in
	   let zs, f, z' = Shape.get_fun z0 in
	   (* evaluation: sum of effects and constraints *)
	   let sf, sk = List.fold_left2
		(fun (f,k) z e ->
			let ez, ef, ek = of_exp env e in
			let _r, z1 = Unify.match_ref_shape z in
			Unify.(z1 =~ ez);
			Effects.(ef + f), Constraints.(ek + k)
		)
		E.(just_var f + f0,k0) zs es in
	   begin match lv_opt with
	   | None    -> sf, sk
	   | Some lv -> with_lval_set env z' sf sk lv
	   end
	(* Oops, unsound :_( *)
	| Cil.Asm _
	-> Effects.none, Constraints.none

let sum_f_k : (Effects.t * Constraints.t) list -> Effects.t * Constraints.t
	= List.fold_left
		(fun (f, k) (f1, k1) -> Effects.(f1 + f), Constraints.(k1 + k))
		(Effects.none, Constraints.none)

(* TODO: CIL Cfg builds a control-flow graph on the AST structure,
   each stmt receives an id, that we can use to map stmt to
   [Effects.t list]. Note that an statement may be a sequence of
   instructions.
 *)

let rec of_stmtkind (env :Env.t)
	: Cil.stmtkind -> Effects.t * Constraints.t
	= function
	| Cil.Instr is
	-> sum_f_k (List.map (of_instr env) is)
	| Cil.Return (e_opt,_loc)
	-> begin match e_opt with
	   | None   -> Effects.none, Constraints.none
	   | Some e -> let _z, f, k = of_exp env e in
				f, k
	   end
	(* this is just control-flow, no effects *)
	| Cil.Goto _
	| Cil.Break _
	| Cil.Continue _
	-> Effects.none, Constraints.none
	(* TODO: still unsupported GCC extension *)
	| Cil.ComputedGoto _
	-> Error.not_implemented()
	| Cil.If (e,b1,b2,_loc)
	-> let _z0, f0, k0 = of_exp env e in
	   let      f1, k1 = of_block env b1 in
	   let      f2, k2 = of_block env b2 in
	   Effects.(f0 + f1 + f2), Constraints.(k0 + k1 + k2)
	| Cil.Switch _
	-> Error.not_implemented()
	(* The last two elements in the tuple refer to CFG instrumentation. *)
	| Cil.Loop (b,_loc,_continue,_break)
	-> of_block env b
	| Cil.Block b
	-> of_block env b
	(* Not interested in supporting these two from MSVC *)
	| Cil.TryFinally _
	| Cil.TryExcept _
	-> Error.not_implemented()

and of_stmt (env :Env.t) (s :Cil.stmt) : E.t * K.t =
	of_stmtkind env Cil.(s.skind)

and of_block (env :Env.t) (b :Cil.block) : E.t * K.t =
	 sum_f_k (List.map (of_stmt env) Cil.(b.bstmts))

let of_fundec (env :Env.t) (k :K.t) (fd :Cil.fundec)
		: shape scheme * K.t =
	let shp', env' = Env.of_fundec env fd in
	let body = Cil.(fd.sbody) in
	let env'' = Env.of_fundec_locals env' fd in
	(* THINK: Maybe we don't need to track constraints but just compute them
	   as the FV of the set of effects computed for the body of the function? *)
	let bf, k1 = of_block env'' body in
	let bf' = observe env' bf in (* FIXME in the paper: not env but env'! *)
	let _,f,_  = Shape.get_fun shp' in
	(* f >= bf' may introduce a recursive subeffecting constraint
	   if the function is recursive.
	   Possible FIX? Create a new fresh effect variable? f' >= bf'?
	   Possible FIX? Remove f from bf'?
	   What about mutually recursive functions?
	 *)
	let k2 = K.add f bf' k1 in
	generalize env k2 shp'

let of_global (env :Env.t)
	: Cil.global -> unit
	= Error.not_implemented()

let of_file : Cil.file -> unit = Error.not_implemented()
