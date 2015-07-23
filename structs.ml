
open Batteries

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
