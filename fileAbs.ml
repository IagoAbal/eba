
open Batteries

open Type

module VarMap = Hashtbl.Make(Utils.Varinfo)

type entry = Var of shape scheme * Effects.t
		   | Fun of shape scheme * FunAbs.t

type t = entry VarMap.t

let create ~no_globals = VarMap.create no_globals

let add_var tbl x sch ef =
	let entry = Var(sch,ef) in
	VarMap.replace tbl x entry

let add_fun tbl x sch fnAbs =
	let entry = Fun(sch,fnAbs) in
	VarMap.replace tbl x entry

let find = VarMap.find

let find_fun tbl fn =
	match find tbl fn with
	| Var _         ->
		Error.panic_with("FileAbs.find_fun: not a function: " ^ Cil.(fn.vname))
	| Fun (sch,abs) -> sch, abs

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
		FunAbs.finalize fnAbs;
		Fun(sch',fnAbs)

let finalize = VarMap.map_inplace (fun _ -> finalize_entry)

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
	FunAbs.fprint out fnAbs

let print_entry out = function
	| (x,Var(sch,ef))     -> print_var out x sch ef
	| (fx,Fun(sch,fnAbs)) -> print_fun out fx sch fnAbs

let fprint out tbl = tbl
	|> List.of_enum % VarMap.enum
	|> List.sort Utils.(compare_on_first Varinfo.loc_of)
	|> List.iter (print_entry out)

let print = fprint stdout

let eprint = fprint stderr
