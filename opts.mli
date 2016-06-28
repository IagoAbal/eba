
module Set : sig

	val gc_stats : bool -> unit

	val save_abs : bool -> unit

	val fp_inlining : bool -> unit

	val dce : bool -> unit

	val dfe : bool -> unit

	val unsafe_casts : bool -> unit

	val externs_do_nothing : bool -> unit

end

module Get : sig

	val gc_stats : unit -> bool

	val save_abs : unit -> bool

	val fp_inlining : unit -> bool

	val dfe : unit -> bool

	val dce : unit -> bool

	val unsafe_casts : unit -> bool

	val externs_do_nothing : unit -> bool

end
