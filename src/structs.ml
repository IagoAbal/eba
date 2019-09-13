open Batteries
open Dolog 

(* Dependency Graph *)
module Dep = struct

(* NB: We use imperative graphs internally,
 *     but the module is effectively pure.
 *)

module G = Graph.Imperative.Digraph.Concrete
	(struct
		type t = Cil.compinfo
		let compare ci1 ci2 = Cil.(compare ci1.cname ci2.cname)
		let hash = Hashtbl.hash
		let equal ci1 ci2 = Cil.(ci1.cname = ci2.cname)
	 end)

(* enumeration of the structs on which a type directly depends *)
let rec structs_in_typ = function
		| Cil.TPtr(ty,_)
		| Cil.TArray(ty,_,_) ->
			structs_in_typ ty
		| Cil.TNamed (ti,_) -> structs_in_typ Cil.(ti.ttype)
		| Cil.TFun(res,args_opt,_,_) ->
			let args =
				Option.(args_opt |? [])
				|> List.enum
				|> Enum.concat_map (structs_in_typ % Tuple3.second)
			in
			Enum.append (structs_in_typ res) args
		| Cil.TComp (ci,_) -> Enum.singleton ci
		| _other -> Enum.empty()

(* enumeration of struct's dependencies *)
let deps_of_struct ci :Cil.compinfo Enum.t =
	Cil.(ci.cfields)
		|> List.enum
		|> Enum.concat_map Cil.(fun {ftype} -> structs_in_typ ftype)

(* enumeration of struct declarations within a C(IL) file *)
let enum_structs file :Cil.compinfo Enum.t =
	(* THINK: We could also consider GCompTagDecl, but then
	 * we have to remove duplicates from the enumeration.
	 * That would avoid a hack in Type.of_struct. This can
	 * be done with Enum.uniq but I should look into its
	 * time and memory complexity.
	 *)
	let is_struct = function
		| Cil.GCompTag (ci,_) -> Some ci
		| _other -> None
	in
	Cil.(file.globals)
		|> List.enum
		|> Enum.filter_map is_struct

(* build a dependency graph of the structs in a file *)
let struct_graph file :G.t =
	let g = G.create () in
	(* add vertices *)
	enum_structs file
		|> Enum.iter (G.add_vertex g);
	(* add edges *)
	enum_structs file
		|> Enum.iter (fun x -> (* for each struct x *)
			let deps = deps_of_struct x in
			deps |> Enum.iter (fun y -> (* for each dependency y *)
				Cil.(G.add_edge g x y)
			)
		);
	g

module SC = Graph.Components.Make(G)

let of_file file = SC.scc_list (struct_graph file)

end

(* Dead struct field elimination *)
module DFE = struct

	open Cil

	(* A map from struct ckey to used fields *)
	type used = (int,string Set.t) Hashtbl.t

	(* Takes note of which struct fields are being used *)
	class usedFieldVisitor (used :used) = object(self)
		inherit nopCilVisitor
		method voffs = function
			| Field(fi,offset') ->
				Hashtbl.modify_def (Set.singleton fi.fname)
					fi.fcomp.ckey (Set.add fi.fname) used;
				DoChildren
			| _else______ ->
				DoChildren
		method vinitoffs = self#voffs
	end

	(* Filters out the unused fields of the given struct (or union). *)
	let filterFields used (ci :compinfo) : fieldinfo list =
		try
			let used_names = Hashtbl.find used ci.ckey in
			ci.cfields |> List.filter (function fi ->
				let is_used = Set.mem fi.fname used_names in
				if not is_used
				then Log.info "UNUSED field %s.%s" ci.cname fi.fname;
				is_used
			)
		with
		(* If the struct (ckey) is not found in `used' that means that no field
		 * of the struct in question is used, right?
		 *)
		| Not_found ->
			Log.info "UNUSED struct %s" ci.cname;
			[]

	class filterFieldsVisitor (used :used) = object(self)
		inherit nopCilVisitor
		method vglob = function
		| GCompTag (ci,_) ->
			ci.cfields <- filterFields used ci;
			SkipChildren
		| _else__________ ->
			SkipChildren
	end

	let of_file file : unit =
		(* populates used map *)
		let used = Hashtbl.create 31 in
		let uVis = new usedFieldVisitor used in
		visitCilFileSameGlobals uVis file;
		(* removes unused fields, preserving the declaration order of the
		 * remaining fields. *)
		let fVis = new filterFieldsVisitor used in
		visitCilFileSameGlobals fVis file

end
