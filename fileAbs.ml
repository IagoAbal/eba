
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

(* THINK: mostly duplicated from Env.pp, refactor? export Env.pp_binding? *)
let pp_var_entry x sch ef :Cil.location * PP.doc =
	let loc = Cil.(x.vdecl) in
	let n = Cil.(x.vname) in
	loc, PP.(!^ n ++ colon ++ Shape.pp sch.body ++ !^ "&" ++ Effects.pp ef)

let pp_fun_entry fx sch fnAbs :Cil.location * PP.doc =
	let loc = Cil.(fx.vdecl) in
	let fn = Cil.(fx.vname) in
	let fun_pp = PP.(
		!^ fn ++ colon ++ Shape.pp sch.body
		+ newline
		+ PP.indent (FunAbs.pp fnAbs)
	) in
	loc, fun_pp

let pp_entry = function
	| (x,Var(sch,ef))     -> pp_var_entry x sch ef
	| (fx,Fun(sch,fnAbs)) -> pp_fun_entry fx sch fnAbs

let pp = Utils.Location.pp_with_loc %
	List.map pp_entry % List.of_enum % VarMap.enum

let to_string = PP.to_string % pp
