
open Batteries

open Type
open PathTree

module L = LazyList

module type Spec = sig
	(* TODO: We may generalize the objects being tracked (here a region)
	 * For instance, some of these checkers could track a set of regions
	 * at a time, reducing traversals and improving locality.
	 *)
	type bug = Region.t
	val name : string
	val doc_of_bug : region -> PP.doc
	val select : FileAbs.t -> Cil.fundec -> shape scheme -> FunAbs.t -> bug L.t
	val trace : bug -> Effects.t -> bool
	(* TODO: We may allow to accumulate state *)
	val testP : bug -> Effects.t -> bool
	val testQ : bug -> Effects.t -> bool
end

module type S = sig
	val in_func : FileAbs.t -> Cil.fundec -> string L.t
end

module Make (A :Spec) : S = struct

	type report = {
		fn    : Cil.varinfo;
		reg   : Region.t;
		loc   : Cil.location;
		trace : path;
	}

	let string_of_report {fn; reg; loc; trace} = PP.(to_string(
		brackets(words A.name) + newline +
		A.doc_of_bug reg ++ !^ "at"
		++ (Utils.Location.pp loc) + newline
		+ !^ "In" ++ !^ Cil.(fn.vname) ++ words "defined at"
		++ (Utils.Location.pp Cil.(fn.vdecl)) + colon + newline
		+ pp_path trace
	))

	let search fd pt r : report L.t =
		let lps = reachable false pt
			~guard:(A.testP r)
			~target:(A.testQ r)
			~trace:(A.trace r)
		in
		let mk_report (s,p,_) = {
			fn = Cil.(fd.svar);
			reg = r;
			loc = s.sloc;
			trace = p;
		} in
		L.map mk_report lps

	let in_func fileAbs fd =
		let fn = Cil.(fd.svar) in
		let fsch, fnAbs = FileAbs.find_fun fileAbs fn in
		let rs = A.select fileAbs fd fsch fnAbs in
		let pt = paths_of fnAbs in
		(* NB: Lazy.t is not thread safe so this cannot be parallelized easily. *)
		let bugs = rs |> L.map (search fd pt) |> L.concat in
		bugs |> L.map string_of_report

end
