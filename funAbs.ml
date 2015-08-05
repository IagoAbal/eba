

open Batteries
open Type

module VarMap = Hashtbl.Make(Utils.Varinfo)
module LocMap = Hashtbl.Make(Utils.Location)

(* TODO: Store the Cil.fundec too *)
(* THINK: Local variables have monomorphic shape schemes... so we could simplify this as vars : shape VarMap.t *)
type t = {
	vars : shape scheme VarMap.t;
	locs : E.t LocMap.t
}

(* TODO: no_vars = formals + locals
         no_locs ~ no_stmts ?
 *)
let create () =
	{ vars = VarMap.create 11
	; locs = LocMap.create 71
	}

let add_var tbl x sch =
	VarMap.replace tbl.vars x sch

let add_vars tbl =
	List.iter (fun (x,sch) -> add_var tbl x sch)

let add_loc tbl loc eff =
	Log.debug "Loc -> effects:\n %s -> %s\n"
		   (Utils.Location.to_string loc)
		   (Effects.to_string eff);
	LocMap.modify_def eff loc (fun f -> E.(eff + f)) tbl.locs

let fv_of tbl =
	let fv1 = VarMap.fold (fun _ sch fvs ->
		Vars.(Scheme.fv_of sch + fvs)
	) tbl.vars Vars.none
	in
	LocMap.fold (fun _ ef fvs ->
		Vars.(Effects.fv_of ef + fvs)
	) tbl.locs fv1

let map_inplace f g tbl =
	VarMap.map_inplace f tbl.vars;
	LocMap.map_inplace g tbl.locs

let zonk = map_inplace
	(fun _x -> Scheme.zonk)
	(fun _loc -> Effects.zonk)

let finalize = map_inplace
	(fun _x -> Scheme.zonk)
	(fun _loc -> Effects.(principal % zonk))

let shape_of tbl =
	VarMap.find tbl.vars

let regions_of tbl = Scheme.regions_in % shape_of tbl

let regions_of_list tbl = Regions.sum % List.map (regions_of tbl)

let effect_of tbl =
	LocMap.find tbl.locs

let sum tbl =
	LocMap.fold (fun _ ef acc -> E.(ef + acc)) tbl.locs E.none

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

let print_steps out tbl = tbl.locs
		 |> LocMap.enum
		 |> List.of_enum
		 |> List.sort Utils.compare_first
		 |> List.iter (print_step out)

let fprint out tbl =
	print_vars out tbl;
	print_steps out tbl

let print = fprint IO.stdout

let eprint = fprint IO.stderr
