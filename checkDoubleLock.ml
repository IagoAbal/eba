
open Batteries

module L = LazyList

open Type
open PathTree

(* Reports *)

type report = {
	fn : Cil.varinfo;
	region : Region.t;
	loc1 : Cil.location;
	loc2 : Cil.location;
	trace : PathTree.path;
}

let pp_report {fn; region; loc1; loc2; trace} =
	let open PP in
	words "Double lock at"
	++ (Utils.Location.pp loc1) ++ !^ "and"
	++ (Utils.Location.pp loc2) + newline
	+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
	++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
	+ !^ "Lock" ++ Region.pp region
	(* + !^ "Variable" ++ !^ Cil.(x.vname) ++ parens(Region.pp region) *)
	(* ++ words "declared at" ++ (Utils.Location.pp Cil.(x.vdecl)) *)
	+ colon + newline
	+ pp_path trace

(* Predicates *)

let unlocks r ef :bool =
	Enum.exists (E.(=.)(E.unlocks r)) (E.enum_principal ef)

let not_unlocks r ef :bool = not (unlocks r ef)

let locks r ef :bool =
	Enum.exists (E.(=.)(E.locks r)) (E.enum_principal ef)

let locks_and_not_unlocks r ef =
	locks r ef && not_unlocks r ef

(* TODO: Rewrite using a L.concat_map (which does not exist as of now) *)
let find_double_lock pt r =
	let lps1 = reachable pt
		~guard:(not_unlocks r)
		~target:(locks_and_not_unlocks r)
	in
	let lpss2 = lps1 |> L.map (fun (_,_,pt') ->
		reachable pt' ~guard:(not_unlocks r) ~target:(locks r)
	) in
	let llpss = L.map2 (fun (l1,p1,_) ->
		L.map (fun (l2,p2,_) -> (l1,l2,p1@p2))
	) lps1 lpss2
	in
	L.concat llpss

(* Main *)

let in_fundec fd fnAbs :report L.t =
	let pt = paths_of fnAbs fd in
	let effects = FunAbs.sum fnAbs  in
	let locks = effects |> E.principal
	                    |> E.filter E.is_locks
	                    |> E.regions
	in
	(* TODO: find from which variables we can access those regions *)
	let pss = locks |> List.of_enum |> List.map (fun r ->
		Log.info  "Analyzing variable region %s\n"
			(Region.to_string r);
		let llps = find_double_lock pt r in
		let mk_report (l1,l2,p) = {
			fn = Cil.(fd.svar);
			region = r;
			loc1 = l1;
			loc2 = l2;
			trace = p;
		} in
		L.map mk_report llps
	) in
	L.flatten pss

let in_file file fileAbs :report L.t =
	let fds = Cil.(file.globals) |> List.filter_map (function
		| Cil.GFun(fd,_) -> Some fd
		| ______________ -> None
	)
	in
	let pss = fds |> List.map (fun fd ->
		(* TODO: FileAbs.find_fun *)
		match FileAbs.find fileAbs Cil.(fd.svar) with
		| FileAbs.Var _ ->
			Error.panic()
		| FileAbs.Fun(_,fnAbs) ->
			Log.info "Analyzing function %s\n" Cil.(fd.svar.vname);
			in_fundec fd fnAbs
	)
	in
	L.flatten pss
