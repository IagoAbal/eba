
type copts = {
	mutable gc_stats : bool;
	mutable save_abs : bool;
}

let opts : copts = {
	gc_stats = false;
	save_abs = false;
}

module Set =
struct

	let gc_stats v = opts.gc_stats <- v

	let save_abs v = opts.save_abs <- v

end

module Get =
struct

	let gc_stats () = opts.gc_stats

	let save_abs () = opts.save_abs

end
