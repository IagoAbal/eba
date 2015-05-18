
open Batteries

module L = LazyList

open Type
open PathTree

(* Reports *)

type report = {
	fn : Cil.varinfo;
	x : Cil.varinfo;
	region : Region.t;
	loc : Cil.location;
	trace : PathTree.path;
}

let pp_report {fn; x; region; loc; trace} = PP.(
	words "Read of uninitialized variable at"
	++ (Utils.Location.pp loc) + newline
	+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
	++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
	+ !^ "Variable" ++ !^ Cil.(x.vname) ++ parens(Region.pp region)
	++ words "declared at" ++ (Utils.Location.pp Cil.(x.vdecl))
	+ colon + newline
	+ pp_path trace
)

(* Predicates *)

let writes r ef :bool =
	Enum.exists (E.(=.)(E.writes r)) (E.enum_principal ef)

let not_writes r ef :bool = not (writes r ef)

let reads r ef :bool =
	Enum.exists (E.(=.)(E.reads r)) (E.enum_principal ef)

let reads_and_not_writes r ef =
	reads r ef && not_writes r ef

(* Main *)

let in_fundec fd fnAbs :report L.t =
	let pt = paths_of fnAbs fd in
	let locals = Cil.(fd.slocals) in
	let pss = locals |> List.map (fun x ->
		let sch = FunAbs.shape_of fnAbs x in
		match sch.body with
		| Shape.Ref(r,_) ->
			Log.info  "Analyzing variable %s (region %s)\n"
				Cil.(x.vname)
				(Region.to_string r);
			let lps = reachable pt
				~guard:(not_writes r)
				~target:(reads_and_not_writes r)
			in
			let mk_report (l,p) = {
				fn = Cil.(fd.svar);
				x;
				region = r;
				loc = l;
				trace = p;
			} in
			L.map mk_report lps
		| __other__ ->
			Error.panic()
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
