
type copts = {
	mutable gc_stats : bool
}

let opts : copts = {
	gc_stats = false
}

module Set =
struct

	let gc_stats v = opts.gc_stats <- v

end

module Get =
struct

	let gc_stats () = opts.gc_stats

end
