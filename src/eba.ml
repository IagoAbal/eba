
open Batteries
open Cmdliner

open Abs

module L = LazyList

type checks = {
	  chk_uninit : bool
	; chk_dlock  : bool
        ; chk_dulock : bool
	; chk_uaf    : bool
	; chk_birq   : bool
}

let run_checks checks file fileAbs :unit =
	let run_check_fun fd in_func =
		let with_warn_out print =
			if Opts.Get.warn_output()
			then File.with_file_out (Cil.(file.fileName) ^ ".warn") print
			else print IO.stdout
		in
		in_func fileAbs fd |> L.iter (fun errmsg ->
		 	with_warn_out (fun out ->
				Printf.fprintf out "\nPotential BUG found:\n%s\n\n" errmsg
			)
		)
	in
	let fds = Cil.(file.globals) |> List.filter_map (function
		| Cil.GFun(fd,_) -> Some fd
		| ______________ -> None
	)
	in
	(* THINK: Too much if-ery? *)
	fds |> List.iter (fun fd ->
		Log.debug "Analyzing function %s\n" Cil.(fd.svar.vname);
		if checks.chk_uninit
		then run_check_fun fd CheckUninitFlow1.in_func;
		if checks.chk_uaf
		then run_check_fun fd CheckUAF.in_func;
		if checks.chk_dlock
		then run_check_fun fd CheckDLockFlow2.in_func;
                if checks.chk_dulock
                     then run_check_fun fd CheckDUnlockFlow2.in_func;
		if checks.chk_birq
		then run_check_fun fd CheckBhOnIrqFlow2.in_func;
	)

let infer_file checks fn =
	let file = Frontc.parse fn () in
	let fileAbs = Infer.of_file file in
	if Opts.Get.save_abs()
	then begin
		let fn_abs = fn ^ ".abs" in
		File.with_file_out fn_abs (fun out -> AFile.fprint out fileAbs)
	end;
	run_checks checks file fileAbs;
	if Opts.Get.gc_stats()
	then begin
		Printf.fprintf stderr "======= GC stats =======\n";
		Gc.print_stat stderr;
		Printf.fprintf stderr "========================\n"
	end

let infer_file_gcc checks args =
	let fn = Gcc.gcc args in
	infer_file checks fn

(* CLI *)

let log_level_of_int = function
	| x when x <= 0 -> Log.ERROR
	| 1 -> Log.WARN
	| 2 -> Log.INFO
	| x -> Log.DEBUG (* x >= 3 *)

let infer_files verbosity
		flag_gcstats flag_saveabs flag_warn_output flag_fake_gcc
		flag_no_dce flag_no_dfe flag_safe_casts flag_externs_do_nothing
		opt_inline_limit opt_loop_limit opt_branch_limit flag_no_path_check
		flag_all_lock_types flag_no_match_lock_exp flag_ignore_writes
		chk_uninit chk_dlock chk_dulock chk_uaf chk_birq
		files =
	(* CIL: do not print #line directives. *)
	Cil.lineDirectiveStyle := None;
	Log.color_on();
	Log.set_log_level (log_level_of_int verbosity);
	Opts.Set.gc_stats flag_gcstats;
	Opts.Set.save_abs flag_saveabs;
	Opts.Set.warn_output flag_warn_output;
	Opts.Set.dce (not flag_no_dce);
	Opts.Set.dfe (not flag_no_dfe);
	Opts.Set.unsafe_casts (not flag_safe_casts);
	Opts.Set.externs_do_nothing flag_externs_do_nothing;
	Opts.Set.inline_limit opt_inline_limit;
	Opts.Set.loop_limit opt_loop_limit;
	Opts.Set.branch_limit opt_branch_limit;
	Opts.Set.path_check (not flag_no_path_check);
	Opts.Set.all_lock_types flag_all_lock_types;
	Opts.Set.match_lock_exp (not flag_no_match_lock_exp);
	Opts.Set.ignore_writes flag_ignore_writes;
	let checks = { chk_uninit; chk_dlock; chk_dulock; chk_uaf; chk_birq } in
	Axioms.load_axioms();
	if flag_fake_gcc
	then infer_file_gcc checks files
	else begin
		List.iter Utils.check_if_file_exists files;
		List.iter (infer_file checks) files
	end

let files = Arg.(non_empty & pos_all string [] & info [] ~docv:"FILE")

(* General *)

(* TODO: Write a Cmdliner.converter for Log.log_level *)
let verbose =
	let doc = "Set the verbosity level." in
	Arg.(value & opt int 0 & info ["v"; "verbose"] ~docv:"LEVEL" ~doc)

let flag_gcstats =
	let doc = "Print GC stats after analyzing a C file." in
	Arg.(value & flag & info ["gc-stats"] ~doc)

let flag_saveabs =
	let doc = "Save effect abstraction to an .abs file." in
	Arg.(value & flag & info ["save-abs"] ~doc)

let flag_warn_output =
	let doc = "Save warns into a .warn file." in
	Arg.(value & flag & info ["warn-output"] ~doc)

let flag_fake_gcc =
	let doc = "Fake GCC and preprocess input file." in
	Arg.(value & flag & info ["fake-gcc"] ~doc)

(* Type inferrer*)

let flag_no_dce =
	let doc = "Do not eliminate dead code." in
	Arg.(value & flag & info ["no-dce"] ~doc)

let flag_no_dfe =
	let doc = "Do not ignore unused fields in structure types (aka dead field elimination)." in
	Arg.(value & flag & info ["no-dfe"] ~doc)

let flag_safe_casts =
	let doc = "Fail on potentially unsafe casts." in
	Arg.(value & flag & info ["safe-casts"] ~doc)

let flag_externs_do_nothing =
	let doc = "Ignore potential side-effects of extern functions." in
	Arg.(value & flag & info ["externs-do-nothing"] ~doc)

(* Model checker *)

let opt_inline_limit =
	let doc = "Inline function calls up to $(docv) times. Provide -1 to prevent inlining but accept some false positives." in
	let def = Opts.Get.inline_limit() in
	Arg.(value & opt int def & info ["inline-limit"] ~docv:"N" ~doc)

let opt_loop_limit =
  let doc = "Take up to $(docv) loop iterations." in
  let def = Opts.Get.loop_limit() in
  Arg.(value & opt int def & info ["loop-limit"] ~docv:"N" ~doc)

let opt_branch_limit =
  let doc = "Take up to $(docv) branch decisions." in
  let def = Opts.Get.branch_limit() in
  Arg.(value & opt int def & info ["branch-limit"] ~docv:"N" ~doc)

let flag_no_path_check =
	let doc = "Do not check path consistency." in
	Arg.(value & flag & info ["no-path-check"] ~doc)

(* Double-Lock bug cheker *)

let flag_all_lock_types =
	let doc = "[Double-Lock] Check all lock types (not only spin locks)." in
	Arg.(value & flag & info ["all-lock-types"] ~doc)

let flag_no_match_lock_exp =
	let doc = "[Double-Lock] Do not use heuristics to match lock object expressions." in
	Arg.(value & flag & info ["no-match-lock-exp"] ~doc)

let flag_ignore_writes =
	let doc = "[Double-Lock] Ignore writes that may affect the lock object." in
	Arg.(value & flag & info ["ignore-writes"] ~doc)

(* Bug chekers *)

let check_uninit =
	let doc = "Check for uses of variables before initialization" in
	Arg.(value & flag & info ["U"; "uninit"] ~doc)

let check_dlock =
	let doc = "Check for double locking" in
	Arg.(value & flag & info ["L"; "dlock"] ~doc)

let check_dulock =
  let doc = "Check for double unlocking" in
  Arg.(value & flag & info ["D";"dunlock"] ~doc)
        
let check_uaf =
	let doc = "Check for use-after-free" in
	Arg.(value & flag & info ["F"; "uaf"] ~doc)

let check_birq =
	let doc = "Check for BH-enabling while IRQs are off" in
	Arg.(value & flag & info ["B"; "bh-irq"] ~doc)

let cmd =
	let doc = "Effect-based analysis of C programs" in
	let man =
	[
		`S "DESCRIPTION";
		`P "Author: Iago Abal <mail@iagoabal.eu>.";

		`P "To preprocess the input use `--fake-gcc' and pass the necessary arguments after `--', as in:";
		`P "eba --fake-gcc -- -Iinclude/ foo.c";
		`P "EBA will extract the `-D', `-include', and `-I' arguments and invoke GCC, any other option will be ignored."
	] in
	Term.(pure infer_files
		$ verbose
		$ flag_gcstats $ flag_saveabs $ flag_warn_output $ flag_fake_gcc
		$ flag_no_dce $ flag_no_dfe $ flag_safe_casts $ flag_externs_do_nothing
		$ opt_inline_limit $ opt_loop_limit $ opt_branch_limit $ flag_no_path_check
		$ flag_all_lock_types $ flag_no_match_lock_exp $ flag_ignore_writes
		$ check_uninit $ check_dlock $ check_dulock $ check_uaf $ check_birq
		$ files),
	Term.info "eba" ~version:"0.1" ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
