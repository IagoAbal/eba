module Opts = Opts.Get

open Batteries
open Type
open Dolog

open Shape
open Abs

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

(* generalize functions shapes *)
(* TODO: solve local subeffecting constraints. *)
let generalize_fun env_fv k r z fnAbs
	: shape scheme * K.t =
	let rr = Region.zonk r in
	let zz = Shape.zonk z in
	AFun.zonk fnAbs;
	let z_fv = Shape.fv_of zz in
	  (* Generalize unconstrained local variables too. *)
	let abs_fv = AFun.fv_of_locals fnAbs in
	let fd_fv = Vars.(z_fv + abs_fv) in
	let vs' = DietFV.diff_vars fd_fv env_fv in
	  (* Prevents incorrect quantification over the function's code region [r]
	   * in case of a function that calls itself.
	   *)
	let vs = Vars.remove_region rr vs' in
	let k1 = K.minus k vs in
	assert(Vars.for_all Shape.is_meta Region.is_meta EffectVar.is_meta vs);
	let sch = Scheme.(ref_of rr (quantify vs zz)) in
	AFun.zonk fnAbs;
	sch, k1

(* THINK: We should not need to operate on Enum.t *)
(* THINK: We can fuse zonk and filter with filter_map *)
let observe env_fv ef :E.t =
    let is_observable = function
		| E.Var x     -> DietFV.mem_effect x env_fv
		| E.Mem(_k,r) -> DietFV.mem_region r env_fv
		| _other      -> true
	in
	let open Effects in
	let filter_observable = Enum.filter (is_observable % uncertain) in
	ef |> zonk |> enum_principal |> filter_observable |> of_enum

(* TODO: We should keep "precision" info associated with
   region variables. E.g. to know if something is the result
   of unsafe pointer arithmetic.
 *)

let of_const : Cil.constant -> shape
	= function
	| Cil.CInt64 _ -> Shape.fresh()
	| Cil.CStr _
	| Cil.CWStr _  -> Shape.fresh_ptr_to Bot
	| Cil.CChr _   -> Bot
	| Cil.CReal _  -> Bot
	| Cil.CEnum _  -> Bot

let of_unop (_env :Env.t) z
		: Cil.unop -> shape * Effects.t
	= function
	| Cil.LNot   -> Shape.Bot, Effects.none
	| __other__  -> z, Effects.none

(* For now, we just pick the shape of the first argument, which in CIL,
   it's always the pointer argument. This may be unsound in some corner
   (rare?) cases, but we are unsound anyways. Better to be precise in the
   majority of cases.

   TODO: PlusPI -> mark that a pointer is the result of arithmetic
		 MinusPP -> warn if after unification the two regions are different
 *)
let of_binop (_env :Env.t) z1 _z2
		: Cil.binop -> shape * Effects.t
	= function
	| Cil.Eq
	| Cil.Ne
	| Cil.Lt
	| Cil.Le
	| Cil.Gt
	| Cil.Ge
	| Cil.LAnd
	| Cil.LOr
		-> Shape.Bot, Effects.none
	| __other__ -> z1, Effects.none

let rec of_exp (env :Env.t)
	: Cil.exp -> shape * Effects.t * K.t
	= function
	| Cil.Const c
	-> of_const c, Effects.none, K.none
	(* Since we flatten array shapes, [StartOf] can be handled
	 * the same way as [Lval]. *)
	| Cil.Lval lv
	| Cil.StartOf lv ->
		let _args, z, f, k = read_lval env lv in
		assert(TypeArgs.is_empty _args);
		z, f, k
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
	   z3, Effects.(f1 + f2 + f3), K.(k1 + k2)
	| Cil.Question (e1,e2,e3,ty) ->
	  let _z1, f1, k1 = of_exp env e1 in
	  let  z2, f2, k2 = of_exp env e2 in
	  let  z3, f3, k3 = of_exp env e3 in
	  Unify.(z2 =~ z3);
	  z2, Effects.(f1 + f2 + f3), K.(k1 + k2 + k3)
	| Cil.CastE (ty,e)
	-> let z, f, k = of_exp env e in
	   let z1 = Unify.match_shape_with_typ z ty in
	   z1, f, k
	| Cil.AddrOf lv
	-> let _, z, f, k = of_lval env lv in
	   Ptr z, f, k
	(* TODO: This is a GCC extension, I hope not a popular one :-) *)
	| Cil.AddrOfLabel _  -> Error.not_implemented("of_exp: addr-of-label")

and read_lval env lv : TypeArgs.t * shape * E.t * K.t =
	let args, z, f, k = of_lval env lv in
	(* TODO: of_lval should always return a ref shape, no need to match? *)
	let r, z0 = Unify.match_ref_shape z in
	let f' =
		 (* If `lv' is a function, it's going to be called.
		  * Otherwise CIL would have inserted an `&'.
		  *)
		E.(f +. if Shape.is_fun z0 then calls r else reads r)
	in
	args, z0, f', k

and of_lval (env :Env.t)
	: Cil.lval -> TypeArgs.t * shape * Effects.t * K.t
	= function (lhost,offset) ->
		let args, z, f, k = of_lhost env lhost in
		let z1, f1, k1 = with_offset env z offset in
		args, z1, Effects.(f + f1), K.(k + k1)

and with_offset (env: Env.t) (z :shape)
	: Cil.offset -> shape * Effects.t * K.t
	= function
	| Cil.NoOffset      -> z, Effects.none, K.none
	(* array indexing *)
	| Cil.Index (e,off) ->
		let _z0, f0, k0 = of_exp env e in
		(* z = ref z1 *)
		let r, z1 = Unify.match_ref_shape z in
		(* z = ref ptr z2 *)
		let z2 = Unify.match_ptr_shape z1 in
		let z3, f1, k1 = with_offset env z2 off in
		z3, Effects.(f0 + f1 +. reads r), K.(k0+k1)
	(* record field *)
	| Cil.Field (fi,off) ->
		(* overall: z ~ ref struct { ...; f : zf; ... } *)
 		let _r, z1 = Unify.match_ref_shape z in
		let zs = Shape.match_struct_shape z1 in
		let zf = Shape.field zs Cil.(fi.fname) in
		with_offset env zf off

and of_lhost (env :Env.t)
	: Cil.lhost -> TypeArgs.t * shape * Effects.t * K.t
	= function
	| Cil.Var x ->
		let sch = Env.find x env in
		let z, args, k = Scheme.instantiate sch in
		(* TODO: assert (is_ref_shape z) ? *)
		assert(Vars.is_empty (Shape.bv_of z));
		args, z, Effects.none, k
	| Cil.Mem e ->
		let z, f, k = of_exp env e in
		let z1 = Unify.match_ptr_shape z in
		TypeArgs.empty, z1, f, k

let with_lval_set (env :Env.t) z f k lv : Effects.t * K.t =
	let _args, z1, f1, k1 = of_lval env lv in
	assert(TypeArgs.is_empty _args);
	let r, z0 = Unify.match_ref_shape z1 in
	Unify.(z0 =~ z);
	Effects.(f + f1 +. writes r), K.(k + k1)

let of_fun fnAbs env loc
	: Cil.exp -> Shape.t * E.t * K.t
	= function
	| Cil.Lval ((Cil.Var _x),Cil.NoOffset as fx) ->
		let args, z, f, k = read_lval env fx in
		AFun.add_call fnAbs loc args;
		z, f, k
	| fn                                         ->
		of_exp env fn

let of_instr fnAbs (env :Env.t)
	: Cil.instr -> Effects.t * K.t
	= function
	| Cil.Set (lv,e,_loc)
	-> let z, f, k = of_exp env e in
	   with_lval_set env z f k lv
	| Cil.Call (lv_opt,fn,es,loc)
	-> (* z' fn(zs) | f *)
	   let z0, f0, k0 = of_fun fnAbs env loc fn in
	   assert(Vars.is_empty (Shape.bv_of z0));
	   let zs, f, z' = Shape.get_fun z0 in
	   let no_args = List.length zs in
	   assert(List.length es >= no_args);
	   let (es_args,es_varargs) = List.split_at no_args es in
	   (* arguments *)
	   let es_args_zs_rev, sf, sk = List.fold_left2
		(fun (ezs,f,k) z e ->
			let ez, ef, ek = of_exp env e in
			assert(Vars.is_empty (Shape.bv_of ez));
			let _r, z1 = Unify.match_ref_shape z in
			Unify.(z1 =~ ez);
			ez::ezs, Effects.(ef + f), K.(ek + k)
		)
		E.([],just_var f + f0,k0) zs es_args in
	   (* extra arguments
	    * hack: We assume that every extra argument is just "fully read",
	    * which works well for bugs like http://vbdb.itu.dk/#bug/linux/1c17e4d.
	    * This is unsound since we assume no other effects.
	    *)
	   let es_zs_rev, sf', sk' = es_varargs |> List.fold_left (fun (ezs,f,k) e ->
			let ez, ef, ek = of_exp env e in
			ez::ezs, E.(fully_read ez + ef + f), K.(ek + k)
	   ) (es_args_zs_rev,sf,sk)
	   in
	   let es_zs = List.rev es_zs_rev in
	   (* extra effect footprint *)
	   let footprint = Axioms.footprint fn z0 es es_zs in
	   let sf'' = E.(footprint + sf') in
	   (* assignment (optional) *)
	   begin match lv_opt with
	   | None    -> sf'', sk'
	   | Some lv -> with_lval_set env z' sf'' sk' lv
	   end
	(* Oops, unsound :_( *)
	| Cil.Asm (_,_,outs,inps,_,_,_) ->
		let fx_in, k_in = List.fold_left (fun (fx,k) (_,_,e) ->
			let _z, fx_e, k_e = of_exp env e in
			E.(fx + fx_e), K.(k + k_e)
		) (Effects.none, K.none) inps
		in
		let fx_out, k_out = List.fold_left (fun (fx,k) (_,_,lv) ->
			let _targs, z1, f1, k1 = of_lval env lv in
			assert(TypeArgs.is_empty _targs);
			let r, _z0 = Unify.match_ref_shape z1 in
			Effects.(fx + f1 +. writes r), K.(k + k1)
		) (Effects.none, K.none) outs
		in
		E.(fx_in + fx_out), K.(k_in + k_out)


let of_instr_log fnAbs env instr =
	let loc = Cil.get_instrLoc instr in
	let f, k = of_instr fnAbs env instr in
	Log.debug "Instr effects:\n %s -> %s\n: %s"
		   (Utils.Location.to_string loc)
		   (Effects.to_string f)
		   (Utils.string_of_cil Cil.d_instr instr);
	assert(Regions.for_all Region.is_meta (Effects.regions f));
	AFun.add_instr_eff fnAbs loc f;
	f, k

let sum_f_k : (Effects.t * K.t) list -> Effects.t * K.t
	= List.fold_left
		(fun (f, k) (f1, k1) -> Effects.(f1 + f), K.(k1 + k))
		(Effects.none, K.none)

let sum_f_k_weak = Tuple2.map1 E.weaken % sum_f_k

(** Obtain a prefix of statements with strictly sequential control flow,
  * for which we can infer must effects.
  *)
let split_stmts (stmts :Cil.stmt list) :Cil.stmt list * Cil.stmt list =
	let is_instr s =
		let open Cil in
		match s.skind with
		| Instr _ -> true
		| _else__ -> false
	in
	let is_return s =
		let open Cil in
		match s.skind with
		| Return _ -> true
		| _else___ -> false
	in
	match stmts with
	| s::[] when is_return s ->
		stmts, []
	| _else ->
		let prefix, rest = List.span is_instr stmts in
		match rest with
		| ret::[] when is_return ret ->
			prefix@[ret], []
		| _else ->
			prefix, rest

(* TODO: CIL Cfg builds a control-flow graph on the AST structure,
   each stmt receives an id, that we can use to map stmt to
   [Effects.t list]. Note that an statement may be a sequence of
   instructions.
 *)

(* THINK: If we transform the code with prepareCFG we don't need to handle [Switch] *)

let rec of_stmtkind (fnAbs :AFun.t) (env :Env.t) (rz :shape)
	: Cil.stmtkind -> Effects.t * K.t
	= function
	| Cil.Instr is ->
		sum_f_k (List.map (of_instr_log fnAbs env) is)
	| Cil.Return (e_opt,loc)
	-> begin match e_opt with
	   | None   ->
		   AFun.add_expr_eff fnAbs loc E.none;
		   Effects.none, K.none
	   | Some e
	   -> let z, f, k = of_exp env e in
		  AFun.add_expr_eff fnAbs loc f;
	      Unify.(z =~ rz);
	      f, k
	   end
	(* this is just control-flow, no effects *)
	| Cil.Goto(_,loc)
	| Cil.Break loc
	| Cil.Continue loc ->
		AFun.add_instr_eff fnAbs loc E.none;
		Effects.none, K.none
	(* TODO: still unsupported GCC extension *)
	| Cil.ComputedGoto _
	-> Error.not_implemented("of_stmtkind: computed-goto")
	| Cil.If (e,b1,b2,loc)
	-> let _z0, f0, k0 = of_exp env e in
	   let      f1, k1 = of_block fnAbs env rz b1 in
	   let      f2, k2 = of_block fnAbs env rz b2 in
	   AFun.add_expr_eff fnAbs loc f0;
	   Effects.(f0 + f1 + f2), K.(k0 + k1 + k2)
	| Cil.Switch _
	-> Error.not_implemented("of_stmtkind: switch")
	(* The last two elements in the tuple refer to CFG instrumentation. *)
	| Cil.Loop (b,_loc,_continue,_break)
	-> of_block fnAbs env rz b
	| Cil.Block b
	-> of_block fnAbs env rz b
	(* Not interested in supporting these two from MSVC *)
	| Cil.TryFinally _
	| Cil.TryExcept _
	-> Error.not_implemented("of_stmtkind: try-finally/except")

and of_stmt (fnAbs :AFun.t) (env :Env.t) (rz :shape) (s :Cil.stmt)
		: E.t * K.t =
	of_stmtkind fnAbs env rz Cil.(s.skind)

and of_block_must fnAbs env rz b : E.t * K.t =
	let head,tail = split_stmts Cil.(b.bstmts) in
	let f1,k1 = sum_f_k (List.map (of_stmt fnAbs env rz) head) in
	let f2,k2 = sum_f_k_weak (List.map (of_stmt fnAbs env rz) tail) in
	E.(f1 + f2), K.(k1 + k2)

and of_block (fnAbs :AFun.t) (env :Env.t) (rz :shape) (b :Cil.block) : E.t * K.t =
	 sum_f_k_weak (List.map (of_stmt fnAbs env rz) Cil.(b.bstmts))

(** Marks uninitialized regions for a given type and shape, see [of_var_no_init]. *)
let rec of_type_no_init ty z :E.t =
	let open Shape in
	match (ty,z) with
	(* Static arrays are automatically allocated but their elements are uninitialized:
	 * Here we're matching (T[e],ref[_] ptr ref[r] _), the first reference is
	 * statically initialized, the second is not.
	 *)
	| (Cil.TArray (ty',Some _,_),Ref(_,Ptr(z'))) ->
		of_type_no_init ty' z'
	| (_, Ref(r,_)) ->
		(* TODO: if it's global or static variable it should be [nulls r] *)
		E.(just (uninits r))
	| (_, _) -> Error.panic_with("variable has non-ref shape")

(** Marks uninitialized regions for variable declarations without initializer. *)
let of_var_no_init x z :E.t = of_type_no_init Cil.(x.vtype) z


let of_fundec_locals env fnAbs (locals :Cil.varinfo list) :E.t * Env.t =
	let locals_bs = Scheme.fresh_bindings locals in
	AFun.add_vars fnAbs locals_bs;
	let ef = List.fold_left (fun ef (x,sch) ->
		let xf = of_var_no_init x Scheme.(sch.body) in
		(* THINK: The implicit effects of local declarations may
		   better be cached in a different way than regular
		   statement effects. *)
		AFun.add_expr_eff fnAbs Cil.(x.vdecl) xf;
		E.(xf + ef)
	) E.none locals_bs
	in
	ef, Env.with_bindings locals_bs env

(** Inference rule for function definitions
  *
  * NB: env must include the function itself (we assume it can be recursive).
  *)
let of_fundec fileAbs (env :Env.t) (k :K.t) (fd :Cil.fundec)
		: shape Scheme.t * K.t * AFun.t =
	(* Eliminate switch, break, and continue ---for now these constructs
	 * are not particularly interesting.
	 *)
	Cil.prepareCFG fd;
	Cil.computeCFGInfo fd false;
	let fnAbs = AFun.create fileAbs fd in
	let fn = Cil.(fd.svar) in
	let shp' = Scheme.((Env.find fn env).body) in (* TODO: should it be instantiated? *)
	let f_r, shp'' = Unify.match_ref_shape shp' in
	let z_args,f,z_res  = Shape.get_fun shp'' in
	let args_bs = List.map2 (fun x y -> x, Scheme.of_shape y)
		Cil.(fd.sformals)
		z_args
	in
	let env' = Env.with_bindings args_bs env in
	AFun.add_vars fnAbs args_bs;
	let lf, env'' = of_fundec_locals env' fnAbs Cil.(fd.slocals) in
	let body = Cil.(fd.sbody) in
	(* THINK: Maybe we don't need to track constraints but just compute them
	   as the FV of the set of effects computed for the body of the function? *)
	let bf, k1 = of_block_must fnAbs env'' z_res body in
	let ff = E.(lf + bf) in
	(* Share FV computation *)
	let env_nofn_fv = Env.(zonk_diet_fv_of (remove fn env)) in
	let env'_fv = List.fold_left DietFV.(fun fvs (_,s) -> union Scheme.(of_scheme (zonk s)) fvs) DietFV.(union (of_shape (zonk shp')) env_nofn_fv) args_bs in
	let ff' = observe env'_fv ff in (* FIXME in the paper: not env but env'! *)
	(* f >= ff' may introduce a recursive subeffecting constraint
	   if the function is recursive.
	   Possible FIX? Create a new fresh effect variable? f' >= ff'?
	   Possible FIX? Remove f from ff'?
	   What about mutually recursive functions?
	 *)
	let k2 = K.add f ff' k1 in
	(* THINK: Maybe we should generalize in of_global *)
	(* NB: if f is captured by the environment, so its lb-effects are.
	 * Failing to do so, and generalizing variables in ff',
	 * another function g() whose effect depends on f, will get
	 * third-party quantified variables into its effect signature,
	 * violating Scheme.t invariant.
	 *)
	let env_nofn_fv2 =
		(* TODO: clean up *)
		let open DietFV in
		if mem_effect f env_nofn_fv
		then union env_nofn_fv (of_effects (Effects.zonk ff'))
		else env_nofn_fv
	in
	let sch, k3 = generalize_fun env_nofn_fv2 k2 f_r shp'' fnAbs in
	sch, k3, fnAbs

let rec of_lv_init env lv :Cil.init -> E.t * K.t = function
	| Cil.SingleInit e ->
		let ze, fe, ke = of_exp env e in
		let f1, k1 = with_lval_set env ze fe ke lv in
		f1, k1
	| Cil.CompoundInit(_,ii) ->
		List.fold_left (fun (f,k) (offset,init) ->
			let lv' = Cil.addOffsetLval offset lv in
			let f1, k1  = of_lv_init env lv' init in
			E.(f1 + f), K.(k1 + k)
		) (E.none, K.none) ii


let of_gvar env x z :Cil.init option -> E.t * K.t = function
	| None      ->
		if Cil.(x.vstorage = Extern)
		then E.none, K.none
		else of_var_no_init x z, K.none
	| Some init -> of_lv_init env (Cil.var x) init

let of_global (fileAbs :AFile.t) (env :Env.t) (k :K.t) : Cil.global -> Env.t * K.t = function
	(* THINK: Do we need to do anything here? CIL has this unrollType helper
	   that should be enough...
	 *)
	| Cil.GType _
	| Cil.GCompTagDecl _
	| Cil.GCompTag _
	| Cil.GEnumTag _
	| Cil.GEnumTagDecl _ -> env, k
	(* extern declaration
	 * NB: extern inline declarations should be accompanied by a definition, so
	 *     we don't generate an axiom for them.
	 *)
	| Cil.GVarDecl (x,_)
			when (Cil.(x.vstorage = Extern && not x.vinline)
			|| Cil.(String.starts_with x.vname "__builtin_")) ->
		let xn = Cil.(x.vname) in
		Log.debug "Extern/builtin declaration: %s\n" xn;
		let x_ax = Axioms.find x in
		let env' = Env.((x,x_ax) +:: env) in
		AFile.add_var fileAbs x x_ax Effects.none;
		env', k
	| Cil.GVarDecl (x,_) -> (* var declaration / fun prototype *)
		let xn = Cil.(x.vname) in
		Log.debug "Variable declaration: %s\n" xn;
		let env', sch_opt = Env.fresh_if_absent x env in
		if Cil.(isFunctionType x.vtype)
		then begin
			sch_opt |> Option.may (fun x_sch ->
				let x_shp = Scheme.get_fun x_sch in
				Constraints.add_to_fun x_shp (Axioms.find_partial x x_shp)
			)
		end;
		AFile.add_var fileAbs x (Env.find x env') Effects.none;
		env', k
	| Cil.GVar (x,ii,_) -> (* variable definition *)
		let xn = Cil.(x.vname) in
		let env', _ = Env.fresh_if_absent x env in
		Log.debug "Global variable %s : %s\n"
			xn Scheme.(to_string (Env.find x env'));
		(* THINK: move to of_init *)
		let sch_x = Env.find x env' in
		let ef, ki = of_gvar env' x Scheme.(sch_x.body) Cil.(ii.init) in
		AFile.add_var fileAbs x sch_x ef;
		env', K.(k + ki)
	| Cil.GFun (fd,_) ->
		let fn = Cil.(fd.svar) in
		Log.debug "In function %s\n" Cil.(fn.vname);
		(* we may know about fn already, if there is any function declaration *)
		let env' =
			if Cil.(fn.vstorage = Extern && not fn.vinline)
			(* Due to my hacky handling of mutually recursive functions (FIXME)
			 * every extern non-inline function declaration is given a polymorphic
			 * axiom by default (unless the user specifies one). When/if the
			 * definition is found, this axiom must be removed from the environment.
			 * Otherwise the presence of a polymorphic axiom in the environment
			 * will break shape inference for recursive functions which is based
			 * on monomorphic recursion! It may make sense to support polymorphic
			 * recursion when the axiom is given by the user, but that is future
			 * work.
			 *)
			then Env.remove fn env
			else env
		in
		let env'', sch_opt = Env.fresh_if_absent fn env' in
		sch_opt |> Option.may (fun fn_sch ->
			let fn_shp = Scheme.get_fun fn_sch in
			Constraints.add_to_fun fn_shp (Axioms.find_partial fn fn_shp)
		);
		(* infer *)
		let sch, k1, fnAbs = of_fundec fileAbs env'' k fd in
		Log.info "Function %s : %s\n" Cil.(fn.vname) Scheme.(to_string sch);
		(* new environment with f generalized,
		 * this overwrites any previous binding.
		 *)
		let env1 = Env.add fn sch env in
		AFile.add_fun fileAbs fn (Env.find fn env1) fnAbs;
		env1, k1
	(* Oooh, we're unsound here :_( *)
	| Cil.GAsm _
	| Cil.GPragma _ -> env, k
	(* Nothing to be done *)
	| Cil.GText _ -> env, k

(* TODO: globinit ? *)
let of_file (file : Cil.file) :AFile.t =
	(* Dead code elimination: C headers tend to include lots of code that is not
	 * used by the translation unit, we therefore remove any code that cannot be
	 * reached from the _extern_ visible code.
	 * TODO: DCE can do a better job if given a complete program, it would be
	 * desirable to check for a main() function, or get it via a flag.
	 *)
	if Opts.dce()
	then Rmtmps.(removeUnusedTemps ?isRoot:(Some isExportedRoot) file);
	(* Dead field elimination: Lots of structure fields are unused, eliminating
	 * them has the nice effect of reducing the amount of free and bound variables
	 * to handle, and the performance boost can be significant.
	 *)
	if Opts.dfe()
	then Structs.DFE.of_file file;
	process_structs file;
	(* TODO: Here we can read axioms *)
	let no_globals = List.length Cil.(file.globals) in
	let fileAbs = AFile.create ~no_globals in
	let env0 = Env.empty in
	let k0 = K.none in
	(* TODO: We need to perform dependency analysis of functions as we do for structs.
	 * We should also collect declarations without associated definition and generate
	 * axioms for those, not having to look at their linkage.
	 *)
	let _env1, _ = List.fold_left
		(fun (env,k) gbl ->	of_global fileAbs env k gbl)
		(env0,k0)
		Cil.(file.globals)
	in
	AFile.finalize fileAbs;
	fileAbs
