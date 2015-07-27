open Batteries
open Type

open Shape

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
	let mtvs = Var.meta_of_list qvs in
	let s = Subst.mk (List.combine qvs mtvs) in
	let shp' = Shape.vsubst s shp in
	let k = Vars.filter Var.is_effect (fv_of shp') in
	shp', k

(* THINK: move this into type because it knows a lot about the internals *)
(* If we generalize meta-type variables we could just write the meta variables and then zonk, instead of substitution. *)
let quantify (r :Region.t) (vs :Vars.t) (z :shape)
	: shape scheme =
	let ys = Vars.to_list vs in
	let xs = Var.bound_of_list ys in
	(* write into vs's *)
	List.iter2 Var.write ys xs;
	let xs' = Vars.zonk_lb (Vars.of_list xs) in
	let z' = Shape.zonk z in
	{ vars = xs'; body = Shape.Ref(r,z') }

(* generalize functions shapes *)
let generalize_fun env k r z fnAbs
	: shape scheme * K.t =
	let rr = Region.zonk r in
	let zz = Shape.zonk z in
	FunAbs.zonk fnAbs;
	let z_fv = Shape.fv_of zz in
	let abs_fv = FunAbs.fv_of fnAbs in
	let fd_fv = Vars.(z_fv + abs_fv) in
	let env_fv = Env.fv_of (Env.zonk env) in
	let vs = Vars.diff fd_fv env_fv in
	let k1 = K.minus k vs in
	let sch = quantify rr vs zz in
	FunAbs.zonk fnAbs;
	sch, k1

let observe (env :Env.t) ef :E.t =
	let env_fv = Env.fv_of (Env.zonk env) in
    let is_observable = function
		| E.Var x     -> Vars.mem (Var.Effect x) env_fv
		| E.Mem(_k,r) -> Vars.mem (Var.Region r) env_fv
		| E.Noret     -> true
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
		let z, f, k = of_lval env lv in
		let r, z0 = Unify.match_ref_shape z in
		let f' =
			(* If `lv' is a function, it's going to be called.
			 * Otherwise CIL would have inserted an `&'. *)
			if Shape.is_fun z0
			then E.(f +. calls r)
			else E.(f +. reads r)
		in
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
	-> let z, f, k = of_lval env lv in
	   Ptr z, f, k
	(* TODO: This is a GCC extension, I hope not a popular one :-) *)
	| Cil.AddrOfLabel _  -> Error.not_implemented()

and of_lval (env :Env.t)
	: Cil.lval -> shape * Effects.t * K.t
	= function (lhost,offset) ->
		let z, f, k = of_lhost env lhost in
		let z1, f1, k1 = with_offset env z offset in
		z1, Effects.(f + f1), K.(k + k1)

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
		z2, Effects.(f0 +. reads r), k0
	(* record field *)
	| Cil.Field _ -> Error.not_implemented()

and of_lhost (env :Env.t)
	: Cil.lhost -> shape * Effects.t * K.t
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

let with_lval_set (env :Env.t) z f k lv : Effects.t * K.t =
	let z1, f1, k1 = of_lval env lv in
	let r, z0 = Unify.match_ref_shape z1 in
	Unify.(z0 =~ z);
	Effects.(f + f1 +. writes r), K.(k + k1)

let of_instr (env :Env.t)
	: Cil.instr -> Effects.t * K.t
	= function
	| Cil.Set (lv,e,_loc)
	-> let z, f, k = of_exp env e in
	   with_lval_set env z f k lv
	| Cil.Call (lv_opt,fn,es,_loc)
	-> (* z' fn(zs) | f *)
	   let z0, f0, k0 = of_exp env fn in
	   let zs, f, z' = Shape.get_fun z0 in
	   let no_args = List.length zs in
	   assert(List.length es >= no_args);
	   let (es_args,es_varargs) = List.split_at no_args es in
	   (* arguments *)
	   let sf, sk = List.fold_left2
		(fun (f,k) z e ->
			let ez, ef, ek = of_exp env e in
			let _r, z1 = Unify.match_ref_shape z in
			Unify.(z1 =~ ez);
			Effects.(ef + f), K.(ek + k)
		)
		E.(just_var f + f0,k0) zs es_args in
	   (* extra arguments
	    * hack: We assume that every extra argument is just "fully read",
	    * which works well for bugs like http://vbdb.itu.dk/#bug/linux/1c17e4d.
	    * This is unsound since we assume no other effects.
	    *)
	   let sf', sk' = es_varargs |> List.fold_left (fun (f,k) e ->
			let ez, ef, ek = of_exp env e in
			E.(fully_read ez + ef + f), K.(ek + k)
	   ) (sf,sk)
	   in
	   (* assignment (optional) *)
	   begin match lv_opt with
	   | None    -> sf', sk'
	   | Some lv -> with_lval_set env z' sf' sk' lv
	   end
	(* Oops, unsound :_( *)
	| Cil.Asm _ ->
		Effects.none, K.none

let of_instr_log fnAbs env instr =
	let loc = Cil.get_instrLoc instr in
	let f, k = of_instr env instr in
	 Log.debug "Instr effects:\n %s -> %s\n"
		   (Utils.Location.to_string loc)
		   (Effects.to_string f);
	FunAbs.add_loc fnAbs loc f;
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

let rec of_stmtkind (fnAbs :FunAbs.t) (env :Env.t) (rz :shape)
	: Cil.stmtkind -> Effects.t * K.t
	= function
	| Cil.Instr is ->
		sum_f_k (List.map (of_instr_log fnAbs env) is)
	| Cil.Return (e_opt,loc)
	-> begin match e_opt with
	   | None   ->
		   FunAbs.add_loc fnAbs loc E.none;
		   Effects.none, K.none
	   | Some e
	   -> let z, f, k = of_exp env e in
		  FunAbs.add_loc fnAbs loc f;
	      Unify.(z =~ rz);
	      f, k
	   end
	(* this is just control-flow, no effects *)
	| Cil.Goto(_,loc)
	| Cil.Break loc
	| Cil.Continue loc ->
		FunAbs.add_loc fnAbs loc E.none;
		Effects.none, K.none
	(* TODO: still unsupported GCC extension *)
	| Cil.ComputedGoto _
	-> Error.not_implemented()
	| Cil.If (e,b1,b2,loc)
	-> let _z0, f0, k0 = of_exp env e in
	   let      f1, k1 = of_block fnAbs env rz b1 in
	   let      f2, k2 = of_block fnAbs env rz b2 in
	   FunAbs.add_loc fnAbs loc f0;
	   Effects.(f0 + f1 + f2), K.(k0 + k1 + k2)
	| Cil.Switch _
	-> Error.not_implemented()
	(* The last two elements in the tuple refer to CFG instrumentation. *)
	| Cil.Loop (b,_loc,_continue,_break)
	-> of_block fnAbs env rz b
	| Cil.Block b
	-> of_block fnAbs env rz b
	(* Not interested in supporting these two from MSVC *)
	| Cil.TryFinally _
	| Cil.TryExcept _
	-> Error.not_implemented()

and of_stmt (fnAbs :FunAbs.t) (env :Env.t) (rz :shape) (s :Cil.stmt)
		: E.t * K.t =
	of_stmtkind fnAbs env rz Cil.(s.skind)

and of_block_must fnAbs env rz b : E.t * K.t =
	let head,tail = split_stmts Cil.(b.bstmts) in
	let f1,k1 = sum_f_k (List.map (of_stmt fnAbs env rz) head) in
	let f2,k2 = sum_f_k_weak (List.map (of_stmt fnAbs env rz) tail) in
	E.(f1 + f2), K.(k1 + k2)

and of_block (fnAbs :FunAbs.t) (env :Env.t) (rz :shape) (b :Cil.block) : E.t * K.t =
	 sum_f_k_weak (List.map (of_stmt fnAbs env rz) Cil.(b.bstmts))

(* Marks uninitialized regions for variable declarations without initializer. *)
let of_var_no_init x z :E.t =
	let open Shape in
	match (Cil.(x.vtype),z) with
	(* Static arrays are automatically allocated but their elements are uninitialized:
	 * Here we're matching (T[e],ref[_] ptr ref[r] _), the first reference is
	 * statically initialized, the second is not.
	 *)
	| (Cil.TArray (_,Some _,_),Ref(_,Ptr(Ref(r,_))))
	| (_, Ref(r,_)) ->
		(* TODO: if it's global or static variable it should be [nulls r] *)
		E.(just (uninits r))
	| (_, _) -> Error.panic_with("variable has non-ref shape")

let of_fundec_locals env fnAbs (locals :Cil.varinfo list) :Env.t =
	let locals_bs = Scheme.fresh_bindings locals in
	FunAbs.add_vars fnAbs locals_bs;
	List.iter (fun (x,sch) ->
		FunAbs.add_loc fnAbs Cil.(x.vdecl) (of_var_no_init x sch.body)
	) locals_bs;
	Env.with_bindings locals_bs env

(** Inference rule for function definitions
  *
  * NB: env must include the function itself (we assume it can be recursive).
  *)
let of_fundec (env :Env.t) (k :K.t) (fd :Cil.fundec)
		: shape scheme * K.t * FunAbs.t =
	let fnAbs = FunAbs.create () in
	let fn = Cil.(fd.svar) in
	let shp' = (Env.find fn env).body in (* TODO: should it be instantiated? *)
	let f_r, shp'' = Unify.match_ref_shape shp' in
	let z_args,f,z_res  = Shape.get_fun shp'' in
	let args_bs = List.map2 (fun x y -> x, Scheme.of_shape y)
		Cil.(fd.sformals)
		z_args
	in
	let env' = Env.with_bindings args_bs env in
	FunAbs.add_vars fnAbs args_bs;
	let env'' = of_fundec_locals env' fnAbs Cil.(fd.slocals) in
	let body = Cil.(fd.sbody) in
	(* THINK: Maybe we don't need to track constraints but just compute them
	   as the FV of the set of effects computed for the body of the function? *)
	let bf, k1 = of_block_must fnAbs env'' z_res body in
	let bf' = observe env' bf in (* FIXME in the paper: not env but env'! *)
	(* f >= bf' may introduce a recursive subeffecting constraint
	   if the function is recursive.
	   Possible FIX? Create a new fresh effect variable? f' >= bf'?
	   Possible FIX? Remove f from bf'?
	   What about mutually recursive functions?
	 *)
	let k2 = K.add f bf' k1 in
	(* THINK: Maybe we should generalize in of_global *)
	let sch, k3 = generalize_fun (Env.remove fn env) k2 f_r shp'' fnAbs in
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
	| None      -> of_var_no_init x z, K.none
	| Some init -> of_lv_init env (Cil.var x) init

let of_global (fileAbs :FileAbs.t) (env :Env.t) (k :K.t) : Cil.global -> Env.t * K.t = function
	(* THINK: Do we need to do anything here? CIL has this unrollType helper
	   that should be enough...
	 *)
	| Cil.GType _ -> env, k
	| Cil.GCompTag _
	| Cil.GCompTagDecl _ -> Error.not_implemented()
	| Cil.GEnumTag _
	| Cil.GEnumTagDecl _ -> env, k
	| Cil.GVarDecl (x,_) ->
		let xn = Cil.(x.vname) in
		Log.debug "Variable declaration: %s\n" xn;
		if Hashtbl.mem Cil.builtinFunctions xn
		then (Log.info "Skipping builtin function: %s\n" xn;
			  env, k)
		else
			let env' = Env.fresh_if_absent_ax x env in
			FileAbs.add_var fileAbs x (Env.find x env') Effects.none;
			env', k
	| Cil.GVar (x,ii,_) ->
		let xn = Cil.(x.vname) in
		let env' = Env.fresh_if_absent x env in
		Log.debug "Global variable %s : %s\n" xn (Shape.to_string (Env.find x env').body);
		(* THINK: move to of_init *)
		let sch_x = Env.find x env' in
		let ef, ki = of_gvar env' x sch_x.body Cil.(ii.init) in
		FileAbs.add_var fileAbs x sch_x ef;
		env', K.(k + ki)
	| Cil.GFun (fd,_) ->
		let fn = Cil.(fd.svar) in
		Log.debug "In function %s\n" Cil.(fn.vname);
		(* we may know about fn already, if there is any function declaration *)
		let env' = Env.fresh_if_absent fn env in
		(* infer *)
		let sch, k1, fnAbs = of_fundec env' k fd in
		Log.info "Function %s : %s\n" Cil.(fn.vname) (Shape.to_string sch.body);
		(* new environment with f generalized,
		 * this overwrites any previous binding.
		 *)
		let env' = Env.add fn sch env in
		FileAbs.add_fun fileAbs fn (Env.find fn env') fnAbs;
		env', k1
	(* Oooh, we're unsound here :_( *)
	| Cil.GAsm _
	| Cil.GPragma _ -> env, k
	(* Nothing to be done *)
	| Cil.GText _ -> env, k

(* TODO: globinit ? *)
let of_file (file : Cil.file) :FileAbs.t =
	(* TODO: Here we can read axioms *)
	let no_globals = List.length Cil.(file.globals) in
	let fileAbs = FileAbs.create ~no_globals in
	let env0 = Env.empty in
	let k0 = K.none in
	let env1, _ = List.fold_left
		(fun (env,k) gbl ->	of_global fileAbs env k gbl)
		(env0,k0)
		Cil.(file.globals)
	in
	Log.debug "Env:\n %s\n" (Env.to_string (Env.zonk env1));
	FileAbs.zonk fileAbs;
	Log.info "FileAbs:\n%s\n" (FileAbs.to_string fileAbs);
	fileAbs
