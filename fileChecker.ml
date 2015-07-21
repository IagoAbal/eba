
open Batteries

open Type

module L = LazyList

module type Spec = sig
	type bug
	val name : string
	val test : Cil.file -> FileAbs.t -> bug L.t
	val string_of_report : Cil.file -> bug -> string
end

module type S = sig
	val in_file : Cil.file -> FileAbs.t -> string L.t
end

module Make (A :Spec) : S = struct

	let in_file file fileAbs =
		let bugs = A.test file fileAbs in
		bugs |> L.map (A.string_of_report file)

end
