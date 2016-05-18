
module Set : sig

	val gc_stats : bool -> unit

	val save_abs : bool -> unit

	val fp_inlining : bool -> unit

end

module Get : sig

	val gc_stats : unit -> bool

	val save_abs : unit -> bool

	val fp_inlining : unit -> bool

end
