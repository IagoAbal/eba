
type copts = {
	mutable gc_stats : bool;
	mutable save_abs : bool;
	mutable fp_inlining : bool;
}

let opts : copts = {
	gc_stats = false;
	save_abs = false;
	fp_inlining = true;
}

module Set =
struct

	let gc_stats v = opts.gc_stats <- v

	let save_abs v = opts.save_abs <- v

	let fp_inlining v = opts.fp_inlining <- v

end

module Get =
struct

	let gc_stats () = opts.gc_stats

	let save_abs () = opts.save_abs

	let fp_inlining () = opts.fp_inlining

end
