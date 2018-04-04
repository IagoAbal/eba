
open Batteries

open Type
open Abs
open PathTree

module L = LazyList

module type Spec = sig
	val name : string

	(** Checker's internal state, eg. memory regions to track. *)
	type st
	val select : AFile.t -> Cil.fundec -> shape scheme -> AFun.t -> st L.t
	val trace : st -> Effects.t -> bool
	val testP : st -> step -> st option
	val testQ : st -> step -> st option

	type bug = Region.t
	val bug_of_st : st -> bug
	val doc_of_bug : bug -> PP.doc
end

module type S = sig
	val in_func : AFile.t -> Cil.fundec -> string L.t
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
			~guard:A.testP
			~target:A.testQ
			~trace:A.trace
			r
		in
		let mk_report (st',s,p,_) = {
			fn = Cil.(fd.svar);
			reg = A.bug_of_st st';
			loc = s.sloc;
			trace = p;
		} in
		L.map mk_report lps

	let in_func fileAbs fd =
		let fn = Cil.(fd.svar) in
		let fsch, fnAbs = Option.get(AFile.find_fun fileAbs fn) in
		let rs = A.select fileAbs fd fsch fnAbs in
		let pt = paths_of fnAbs in
		(* NB: Lazy.t is not thread safe so this cannot be parallelized easily. *)
		let bugs = rs |> L.map (search fd pt) |> L.concat in
		bugs |> L.map string_of_report

end
