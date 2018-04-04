
module Dep : sig
	(** SCC of the structs declarations in a C(IL) file *)
	val of_file : Cil.file -> Cil.compinfo list list
end

module DFE : sig
	(** Remove unused structure fields in a C(IL) file *)
	val of_file : Cil.file -> unit
end
