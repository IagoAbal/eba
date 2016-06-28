
type copts = {
	mutable gc_stats : bool;
	mutable save_abs : bool;
	mutable fp_inlining : bool;
	mutable dce : bool;
	mutable dfe : bool;
	mutable unsafe_casts : bool;
	mutable externs_do_nothing : bool;
}

let opts : copts = {
	gc_stats = false;
	save_abs = false;
	fp_inlining = true;
	dce = true;
	dfe = true;
	unsafe_casts = true;
	externs_do_nothing = false;
}

module Set =
struct

	let gc_stats v = opts.gc_stats <- v

	let save_abs v = opts.save_abs <- v

	let fp_inlining v = opts.fp_inlining <- v

	let dce v = opts.dce <- v

	let dfe v = opts.dfe <- v

	let unsafe_casts v = opts.unsafe_casts <- v

	let externs_do_nothing v = opts.externs_do_nothing <- v

end

module Get =
struct

	let gc_stats () = opts.gc_stats

	let save_abs () = opts.save_abs

	let fp_inlining () = opts.fp_inlining

	let dce () = opts.dce

	let dfe () = opts.dfe

	let unsafe_casts () = opts.unsafe_casts

	let externs_do_nothing () = opts.externs_do_nothing

end
