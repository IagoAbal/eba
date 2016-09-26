
open Batteries

open Type

(* [OCaml Quirks]
 * Apparently if we name this module `File' rather than `AFile', in the scope of
 * `Fun' signature `File' will refer to `Batteries.File' rather than to `Abs.File'.
 *)

module rec AFile : sig

	type t

	val create : no_globals:int -> t

	val add_var : t -> Cil.varinfo -> shape scheme -> Effects.t -> unit

	val add_fun : t -> Cil.varinfo -> shape scheme -> AFun.t -> unit

	val find_var : t -> Cil.varinfo -> shape option

	val has_fun : t -> Cil.varinfo -> bool

	val find_fun : t -> Cil.varinfo -> (shape scheme * AFun.t) option

	val gvar_regions : t -> Regions.t * Effects.t

	val shape_of : t -> Cil.varinfo -> shape scheme

	val effect_of : t -> Cil.varinfo -> Effects.t

	val sum : t -> Effects.t

	val finalize : t -> unit

	(** Instantiate fn's abstraction with the given type arguments.
	 *
	 * NB: It does NOT instantiate C formal arguments.
	 *)
	val inst_fun : t -> fn:Cil.varinfo -> TypeArgs.t -> AFun.t option

	(** Print to stdout *)
	val print : t -> unit

	(** Print to stderr *)
	val eprint : t -> unit

	val fprint : unit IO.output -> t -> unit

end = struct

	module VarMap = Hashtbl.Make(Utils.Varinfo)

	type entry = Var of shape scheme * Effects.t
			   | Fun of shape scheme * AFun.t

	type t = entry VarMap.t

	let create ~no_globals = VarMap.create no_globals

	let add_var tbl x sch ef =
		let entry = Var(sch,ef) in
		VarMap.replace tbl x entry

	let add_fun tbl x sch fnAbs =
		let entry = Fun(sch,fnAbs) in
		VarMap.replace tbl x entry

	let find = VarMap.find

	let find_var tbl x =
		match VarMap.Exceptionless.find tbl x with
		| None              -> None
		| Some(Fun _)       ->
			Log.warn "File.find_var: not a variable: %s" Cil.(x.vname);
			None
		| Some(Var (sch,_)) ->
			assert (QV.is_empty Scheme.(sch.vars));
			Some Scheme.(sch.body)

	let has_fun tbl fn =
		match VarMap.Exceptionless.find tbl fn with
		| None         -> false
		| Some (Var _) ->
			Log.error "File.has_fun: not a function: %s" Cil.(fn.vname);
			false
		| Some (Fun _) -> true

	let find_fun tbl fn =
		match VarMap.Exceptionless.find tbl fn with
		| None                ->
			None
		| Some(Var _)         ->
			Log.error "File.find_fun: not a function: %s" Cil.(fn.vname);
			None
		| Some(Fun (sch,abs)) -> Some(sch, abs)

	let shape_of tbl x =
		match find tbl x with
		| Var(sch,_)
		| Fun(sch,_) -> sch

	let gvar_regions tbl =
		VarMap.fold (fun _ entry (rs,fs as acc) ->
			match entry with
			| Var(sch,ef) ->
				Regions.(Scheme.regions_in sch + rs), Effects.(fs + ef)
			| Fun _       -> acc
		) tbl (Regions.none,Effects.none)

	let effects_of_entry = function
		| Var(_,ef)      -> ef
		| Fun(sch,fnAbs) -> E.of_enum (Scheme.effects_of_fun sch)

	let effect_of tbl x =
		effects_of_entry(find tbl x)

	(* TODO: This should be precomputed *)
	let sum tbl =
		VarMap.fold (fun _ entry acc ->
			let ef = effects_of_entry entry in
			E.(ef + acc)
		) tbl E.none

	let finalize_entry = function
		| Var(sch,ef) ->
			let sch' = Scheme.zonk sch in
			let ef' = Effects.(principal (zonk ef)) in
			Var(sch',ef')
		| Fun(sch,fnAbs) ->
			let sch' = Scheme.zonk sch in
			AFun.finalize fnAbs;
			Fun(sch',fnAbs)

	let finalize = VarMap.map_inplace (fun _ -> finalize_entry)

	let inst_fun tbl ~fn targs =
		let open Option.Infix in
		find_fun tbl fn >>= fun (sch, abs) ->
		let params, s = QV.instantiate Scheme.(sch.vars) in
		TypeArgs.write params targs;
		let abs' = AFun.vsubst s abs in
		AFun.finalize abs';
		Some abs'

	let print_var out x s f =
		let loc = Utils.Location.to_string Cil.(x.vdecl) in
		let name = Utils.cyan Cil.(x.vname) in
		let shp = Scheme.to_string s in
		let eff = Effects.to_string f in
		Printf.fprintf out "\n%s: %s : %s & %s\n" loc name shp eff

	let print_fun out f s fnAbs =
		let loc = Utils.Location.to_string Cil.(f.vdecl) in
		let fn = Utils.cyan Cil.(f.vname) in
		let sch = Scheme.to_string s in
		Printf.fprintf out "\n%s: %s : %s\n" loc fn sch;
		AFun.fprint out fnAbs

	let print_entry out = function
		| (x,Var(sch,ef))     -> print_var out x sch ef
		| (fx,Fun(sch,fnAbs)) -> print_fun out fx sch fnAbs

	let fprint out tbl = tbl
		|> List.of_enum % VarMap.enum
		|> List.sort Utils.(compare_on_first Varinfo.loc_of)
		|> List.iter (print_entry out)

	let print = fprint stdout

	let eprint = fprint stderr

end

and AFun : sig

	(* TODO: Create immutable wrapper *)

	open Batteries

	open Type

	(** Mapping of variables to shapes, and locations to effects.
	 *
	 * We track the effects of key location in a C file,
	 * those corresponding to C statements.
	 *)
	type t

	val create : AFile.t -> Cil.fundec -> t

	val fundec : t -> Cil.fundec

	val file : t -> AFile.t

	val add_var : t -> Cil.varinfo -> shape scheme -> unit

	val add_vars : t -> (Cil.varinfo * shape scheme) list -> unit

	(** Look up a variable name in the function (or else, in the file) environment. *)
	val find_var : t -> Cil.varinfo -> shape option

	val add_instr_eff : t -> Cil.location -> Effects.t -> unit

	val add_expr_eff : t -> Cil.location -> Effects.t -> unit

	val add_call : t -> Cil.location -> TypeArgs.t -> unit

	val fv_of_locals : t -> Vars.t

	val fv_of : t -> Vars.t

	val vsubst : Subst.t -> t -> t

	val zonk : t -> unit

	val finalize : t -> unit

	(** Look up a variable or function name in the function (or else, in the file) environment. *)
	val shape_of : t -> Cil.varinfo -> shape scheme

	val regions_of : t -> Cil.varinfo -> Regions.t

	val regions_of_list : t -> Cil.varinfo list -> Regions.t

	val effect_of_instr : t -> Cil.location -> Effects.t

	val effect_of_expr : t -> Cil.location -> Effects.t

	val uninit_locals : t -> Cil.fundec -> Regions.t

	val call : t -> fn:Cil.varinfo -> Cil.location -> t option

	(* TODO: This should be precomputed for an immutable Fun.t *)
	val sum : t -> Effects.t

	(** Print to stdout *)
	val print : t -> unit

	(** Print to stderr *)
	val eprint : t -> unit

	val fprint : unit IO.output -> t -> unit

end = struct

	module VarMap = Hashtbl.Make(Utils.Varinfo)
	module LocMap = Hashtbl.Make(Utils.Location)

	(** NOTE [Location-to-Effect mapping]
	 *
	 * In CIL expressions are side-effect free, thus something like this:
	 *
	 *     if (f(x)) ...
	 *
	 * is converted into:
	 *
	 *     tmp = f(x);
	 *     if (tmp) ...
	 *
	 * The problem for us is that both the function call and the if test have the
	 * same source location assigned. If we simply map locations to effects, we
	 * will record that `tmp' has the side-effects of calling `f', and that may
	 * cause some weird results (e.g. a double lock bug report).
	 *
	 * The solution is simple, we just maintain two separate mappings, one for
	 * instructions, and another for expressions. Expressions are a bit boring,
	 * and should only have read effects.
	 *
	 * TODO: This is actually part of a more general problem of the CIL front-end:
	 *
	 *           for (e1; e2; e3) ...
	 *
	 *       In a for-loop construct all the subexpressions receive the same source
	 *       location, so the effects of all of them get confused.
	 *
	 *       The best solution for this problem that I can think of, requires that
	 *       we generate a new effect-annotated AST, rather than just keeping a
	 *       separate map assigning effects to locations.
	 *)

	(* THINK: Local variables have monomorphic shape schemes... so we could simplify this as vars : shape VarMap.t *)
	type t = {
		 (* Associated file. *)
		file : AFile.t;
		 (* Function definition. *)
		fdec : Cil.fundec;
		 (* Formal parameters and local variables. *)
		vars : shape scheme VarMap.t;
		 (* Effects of a CIL instruction. *)
		ieffs : E.t LocMap.t;
		 (* Effects of a CIL expression. *)
		eeffs : E.t LocMap.t;
		 (* Type arguments of a function call. *)
		call : TypeArgs.t LocMap.t;
	}

	(* TODO: no_vars = formals + locals
	         no_locs ~ no_stmts ?
	 *)
	let create fileAbs fd =
		{ file = fileAbs
		; fdec = fd
		; vars = VarMap.create 3
		; ieffs = LocMap.create 19
		; eeffs = LocMap.create 19
		; call = LocMap.create 19
		}

	let fundec tbl = tbl.fdec

	let file fna = fna.file

	let add_var tbl x sch =
		VarMap.replace tbl.vars x sch

	let add_vars tbl =
		List.iter (fun (x,sch) -> add_var tbl x sch)

	let find_local_var fna x =
		Option.map (fun sch ->
				let open Scheme in
				assert(QV.is_empty sch.vars);
				sch.body
			)
			(VarMap.Exceptionless.find fna.vars x)

	let find_var fna x =
		Utils.Option.(find_local_var fna <|> AFile.find_var fna.file) x

	let add_instr_eff tbl loc eff =
		(* Log.debug "Loc -> effects:\n %s -> %s\n"
			   (Utils.Location.to_string loc)
			   (Effects.to_string eff); *)
		(* TODO: assert(Vars.is_empty (Effects.bv_of eff)); *)
		LocMap.modify_def eff loc (fun f -> E.(eff + f)) tbl.ieffs

	let add_expr_eff tbl loc eff =
		LocMap.modify_def eff loc (fun f -> E.(eff + f)) tbl.eeffs

	let add_call tbl loc args =
		LocMap.replace tbl.call loc args

	let args_of_call tbl =
		LocMap.Exceptionless.find tbl.call

	let enum_effs tbl =
		Enum.append (LocMap.enum tbl.ieffs) (LocMap.enum tbl.eeffs)

	let fv_of_locals tbl =
		VarMap.fold (fun _ sch fvs ->
			Vars.(Scheme.fv_of sch + fvs)
		) tbl.vars Vars.none

	let fv_of_calls tbl =
		LocMap.fold (fun _ targs fvs ->
			Vars.(TypeArgs.fv_of targs + fvs)
		) tbl.call Vars.none

	(* fv_of = fv_of_locals + effect variables from function calls *)
	let fv_of tbl =
		let fvs = Vars.(fv_of_locals tbl + fv_of_calls tbl) in
		assert(
			enum_effs tbl |> Enum.for_all (fun (_, ef) ->
				Vars.subset (Effects.fv_of ef) fvs
			)
		);
		fvs

	let map f g h tbl =
		{ tbl with
		  vars = VarMap.map f tbl.vars
		; ieffs = LocMap.map g tbl.ieffs
		; eeffs = LocMap.map g tbl.eeffs
		; call = LocMap.map h tbl.call
		}

	(* TODO: This should create a new read-write FunAbs *)
	let vsubst s = map
		(fun _x sch -> Scheme.(of_shape (Shape.vsubst s sch.body)))
		(fun _l -> Effects.vsubst s)
		(fun _l -> TypeArgs.vsubst s)

	let map_inplace f g h tbl =
		VarMap.map_inplace f tbl.vars;
		LocMap.map_inplace g tbl.ieffs;
		LocMap.map_inplace g tbl.eeffs;
		LocMap.map_inplace h tbl.call

	let zonk = map_inplace
		(fun _x -> Scheme.zonk)
		(fun _loc -> Effects.zonk)
		(fun _loc ts ->
			TypeArgs.zonk ts
		)

	(* TODO: This should freeze a FunAbs becoming read-only *)
	let finalize = map_inplace
		(fun _x -> Scheme.zonk)
		(fun _loc -> Effects.(principal % zonk))
		(fun _loc -> TypeArgs.zonk)

	let shape_of fna =
		try
			VarMap.find fna.vars
		with
		| Not_found -> AFile.shape_of fna.file

	let regions_of tbl = Scheme.regions_in % shape_of tbl

	let regions_of_list tbl = Regions.sum % List.map (regions_of tbl)

	let effect_of_instr tbl =
		LocMap.find tbl.ieffs

	let effect_of_expr tbl =
		LocMap.find tbl.eeffs

	let effect_of_any tbl loc =
		E.(effect_of_instr tbl loc + effect_of_expr tbl loc)

	let call fna ~fn loc =
		let open Option.Infix in
		args_of_call fna loc >>= fun targs ->
		AFile.inst_fun fna.file fn targs

	let sum tbl =
		let e_sum = LocMap.fold (fun _ ef acc -> E.(ef + acc)) tbl.eeffs E.none in
		LocMap.fold (fun _ ef acc -> E.(ef + acc)) tbl.ieffs e_sum

	(* TODO: should be cached *)
	let effect_of_local_decl tbl fd :E.t =
		let open Cil in
		List.fold_left E.(fun fs x -> effect_of_any tbl x.vdecl + fs)
			E.none
			fd.slocals

	let uninit_locals tbl fd :Regions.t =
		let open Effects in
		effect_of_local_decl tbl fd
			 |> filter is_uninits
			 |> regions

	let aliased _ _ = Error.not_implemented "Fun.aliased"

	let points_to _ _ = Error.not_implemented "Fun.points_to"

	let print_var out (x,sch) =
		let loc = Utils.Location.to_string Cil.(x.vdecl) in
		let name = Utils.cyan (Cil.(x.vname)) in
		let shp = Shape.to_string Scheme.(sch.body) in
		Printf.fprintf out "%s: %s : %s\n" loc name shp

	let print_vars out tbl = tbl.vars
			 |> VarMap.enum
			 |> List.of_enum
			 |> List.sort Utils.(compare_on_first Varinfo.loc_of)
			 |> List.iter (print_var out)

	let print_step out (l,f) =
		let loc = Utils.Location.to_string l in
		let ef = Effects.to_string f in
		Printf.fprintf out "%s -> %s\n" loc ef

	let print_steps out tbl = enum_effs tbl
			 |> List.of_enum
			 |> List.sort Utils.compare_first
			 |> List.iter (print_step out)

	(* There is nothing pretty to print unless we save more information,
	 * so this is mostly for debugging.
	 *)
	let print_call out (l,ts) =
		let loc = Utils.Location.to_string l in
		Printf.fprintf out "%s ->" loc;
		TypeArgs.fprint out ts

	let print_calls out tbl = tbl.call
			 |> LocMap.enum
			 |> List.of_enum
			 |> List.sort Utils.compare_first
			 |> List.iter (print_call out)

	let fprint out tbl =
		print_vars out tbl;
		print_steps out tbl

	let print = fprint IO.stdout

	let eprint = fprint IO.stderr

end
