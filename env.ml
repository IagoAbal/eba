
open Batteries
open Type

(* THINK: We could keep global and local bindings separated, which may
   allow for some optimizations. For instance, we could incrementally
   compute (and zonk) global FVs, which is needed every time we infer
   the signature of a function definition.
 *)

module VarMap = Map.Make(
	struct
		type t = Cil.varinfo
		let compare x y = Cil.(compare x.vid y.vid)
	end)

type t = shape scheme VarMap.t

let empty = VarMap.empty

let cardinal = VarMap.cardinal

let find_opt = VarMap.Exceptionless.find

let find x (env :t) :shape scheme =
	match find_opt x env with
	| Some sch -> sch
	| None     ->
		Error.panic_with("Env.find: not found: " ^ Cil.(x.vname))

let add = VarMap.add

let remove = VarMap.remove

let (+::) : Cil.varinfo * shape scheme -> t -> t =
	fun (x,ty) -> add x ty

let (+>) (env1 :t) (env2 :t) :t =
	VarMap.fold VarMap.add env1 env2

let with_fresh x env :t =
	let z = Shape.ref_of Cil.(x.vtype) in
	let sch = Scheme.({ vars = Vars.none; body = z }) in
	(x,sch) +:: env

let fresh_if_absent x env :t =
	match find_opt x env with
	| None ->
		with_fresh x env
	| Some _ ->
		env

(* TODO: Should check that the axiom is compatible with the function's signature. *)
let fresh_if_absent_ax (x :Cil.varinfo) (env :t) :t =
	let is_extern = Cil.(x.vstorage = Extern) in
	if is_extern
	then
		match Axioms.find Cil.(x.vname) with
		| Some sch ->
			(x,sch) +:: env
		| None ->
			fresh_if_absent x env
	else
		fresh_if_absent x env

let of_bindings :(Cil.varinfo * shape scheme) list -> t =
	VarMap.of_enum % List.enum

let with_bindings bs env :t = of_bindings bs +> env

let zonk :t -> t =
	let open Scheme in
	let zonk_sch {vars; body} = {vars; body = Shape.zonk body}  in
	VarMap.map zonk_sch

let fv_of env :Vars.t =
	let open Scheme in
	VarMap.fold (fun _ {body = shp} ->
		Vars.union (Shape.fv_of shp)
	) env Vars.empty

let pp =
	let pp_binding (x,sch) =
		PP.(!^ Cil.(x.vname) ++ colon ++ Shape.pp Scheme.(sch.body))
	in
	PP.(separate newline % List.map pp_binding % VarMap.bindings)

let to_string = PP.to_string % pp
