
module Set : sig

	val gc_stats : bool -> unit

	val save_abs : bool -> unit

	val dce : bool -> unit

	val dfe : bool -> unit

	val unsafe_casts : bool -> unit

	val externs_do_nothing : bool -> unit

	val inline_limit : int -> unit

	val loop_limit : int -> unit

	val branch_limit : int -> unit

	val path_check : bool -> unit

end

module Get : sig

	val gc_stats : unit -> bool

	val save_abs : unit -> bool

	val dfe : unit -> bool

	val dce : unit -> bool

	val unsafe_casts : unit -> bool

	val externs_do_nothing : unit -> bool

	val inline_limit : unit -> int

	val loop_limit : unit -> int

	val branch_limit : unit -> int

	val path_check : unit -> bool

end
