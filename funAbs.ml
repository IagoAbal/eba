

open Batteries
open Type

module VarMap = Hashtbl.Make(Utils.Varinfo)
module LocMap = Hashtbl.Make(Utils.Location)

(* THINK: Local variables have monomorphic shape schemes... so we could simplify this as vars : shape VarMap.t *)
type t = {
	 (* Function definition. *)
	fdec : Cil.fundec;
	 (* Formal parameters and local variables. *)
	vars : shape scheme VarMap.t;
	 (* Effects of a CIL instruction. *)
	effs : E.t LocMap.t;
	 (* Type arguments of a function call. *)
	call : TypeArgs.t LocMap.t;
}

(* TODO: no_vars = formals + locals
         no_locs ~ no_stmts ?
 *)
let create fd =
	{ fdec = fd
	; vars = VarMap.create 3
	; effs = LocMap.create 19
	; call = LocMap.create 19
	}

let fundec tbl = tbl.fdec

let add_var tbl x sch =
	VarMap.replace tbl.vars x sch

let add_vars tbl =
	List.iter (fun (x,sch) -> add_var tbl x sch)

let find_var tbl x =
	Option.map (fun sch ->
			let open Scheme in
			assert(QV.is_empty sch.vars);
			sch.body
		)
		(VarMap.Exceptionless.find tbl.vars x)

let add_loc tbl loc eff =
	Log.debug "Loc -> effects:\n %s -> %s\n"
		   (Utils.Location.to_string loc)
		   (Effects.to_string eff);
	(* TODO: assert(Vars.is_empty (Effects.bv_of eff)); *)
	LocMap.modify_def eff loc (fun f -> E.(eff + f)) tbl.effs

let add_call tbl loc args =
	LocMap.replace tbl.call loc args

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
		tbl.effs |> LocMap.enum |> Enum.for_all (fun (_, ef) ->
			Vars.subset (Effects.fv_of ef) fvs
		)
	);
	fvs

let map f g h tbl =
	{ tbl with
	  vars = VarMap.map f tbl.vars
	; effs = LocMap.map g tbl.effs
	; call = LocMap.map h tbl.call
	}

(* TODO: This should create a new read-write FunAbs *)
let vsubst s = map
	(fun _x sch -> Scheme.(of_shape (Shape.vsubst s sch.body)))
	(fun _l -> Effects.vsubst s)
	(fun _l -> TypeArgs.vsubst s)

let map_inplace f g h tbl =
	VarMap.map_inplace f tbl.vars;
	LocMap.map_inplace g tbl.effs;
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

let shape_of tbl =
	VarMap.find tbl.vars

let regions_of tbl = Scheme.regions_in % shape_of tbl

let regions_of_list tbl = Regions.sum % List.map (regions_of tbl)

let effect_of tbl =
	LocMap.find tbl.effs

let args_of_call tbl =
	LocMap.Exceptionless.find tbl.call

let sum tbl =
	LocMap.fold (fun _ ef acc -> E.(ef + acc)) tbl.effs E.none

(* TODO: should be cached *)
let effect_of_local_decl tbl fd :E.t =
	let open Cil in
	List.fold_left E.(fun fs x -> effect_of tbl x.vdecl + fs)
		E.none
		fd.slocals

let uninit_locals tbl fd :Regions.t =
	let open Effects in
	effect_of_local_decl tbl fd
		 |> filter is_uninits
		 |> regions

let aliased _ _ = Error.not_implemented "FunAbs.aliased"

let points_to _ _ = Error.not_implemented "FunAbs.points_to"

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

let print_steps out tbl = tbl.effs
		 |> LocMap.enum
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
