
open Batteries

open Type

module L = LazyList

module type FiType = sig
	type bug
	val name : string
	val test : FileAbs.t -> Cil.fundec -> FunAbs.t -> shape scheme -> bug L.t
	val string_of_report : Cil.fundec -> bug -> string
end

module type S = sig
	val in_func : FileAbs.t -> Cil.fundec -> string L.t
end

module Make (A :FiType) : S = struct

	let in_func fileAbs fd =
		let fn = Cil.(fd.svar) in
		let fsch, fnAbs = FileAbs.find_fun fileAbs fn in
		let bugs = A.test fileAbs fd fnAbs fsch in
		bugs |> L.map (A.string_of_report fd)

end
