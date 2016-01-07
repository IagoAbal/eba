
open Batteries
open Cmdliner

module L = LazyList

type checks = {
	  chk_uninit : bool
	; chk_noret  : bool
	; chk_dlock  : bool
	; chk_birq   : bool
}

let run_checks checks file fileAbs :unit =
	let run_check_file in_file =
		 in_file file fileAbs |> L.iter (function errmsg ->
			Printf.printf "Potential BUG found:\n%s\n\n" errmsg
		 )
	in
	let run_check_fun fd in_func =
		 in_func fileAbs fd |> L.iter (function errmsg ->
			Printf.printf "Potential BUG found:\n%s\n\n" errmsg
		 )
	in
	let fds = Cil.(file.globals) |> List.filter_map (function
		| Cil.GFun(fd,_) -> Some fd
		| ______________ -> None
	)
	in
	(* THINK: Too much if-ery? *)
	if checks.chk_uninit
	then run_check_file CheckUninitFile.in_file;
	fds |> List.iter (fun fd ->
		Log.debug "Analyzing function %s\n" Cil.(fd.svar.vname);
		if checks.chk_noret
		then run_check_fun fd CheckFiNoret.in_func;
		if checks.chk_uninit
		then run_check_fun fd CheckUninitFlow1.in_func;
		if checks.chk_dlock
		then run_check_fun fd CheckDLockFlow2.in_func;
		if checks.chk_birq
		then run_check_fun fd CheckBhOnIrqFlow2.in_func;
	)

let infer_file checks fn =
	let file = Frontc.parse fn () in
	let fileAbs = Infer.of_file file in
	run_checks checks file fileAbs


(* CLI *)

let log_level_of_int = function
	| x when x <= 0 -> Log.ERROR
	| 1 -> Log.WARN
	| 2 -> Log.INFO
	| x -> Log.DEBUG (* x >= 3 *)

let infer_files verbosity chk_uninit chk_noret chk_dlock chk_birq files =
	Log.color_on();
	Log.set_log_level (log_level_of_int verbosity);
	let checks = { chk_uninit; chk_noret; chk_dlock; chk_birq } in
	List.iter (infer_file checks) files

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE")

(* TODO: Write a Cmdliner.converter for Log.log_level *)
let verbose =
	let doc = "Set the verbosity level." in
	Arg.(value & opt int 0 & info ["v"; "verbose"] ~docv:"LEVEL" ~doc)

let check_uninit =
	let doc = "Check for uses of variables before initialization" in
	Arg.(value & flag & info ["U"; "uninit"] ~doc)

let check_noret =
	let doc = "Check for undeclared non-returning functions" in
	Arg.(value & flag & info ["N"; "noret"] ~doc)

let check_dlock =
	let doc = "Check for double locking" in
	Arg.(value & flag & info ["L"; "dlock"] ~doc)

let check_birq =
	let doc = "Check for BH-enabling while IRQs are off" in
	Arg.(value & flag & info ["B"; "bh-irq"] ~doc)

let cmd =
	let doc = "Effect-based analysis of C programs" in
	let man = [ `S "DESCRIPTION"; `P "Author: Iago Abal <mail@iagoabal.eu>."; ] in
	Term.(pure infer_files $ verbose $ check_uninit $ check_noret $ check_dlock $ check_birq $ files),
	Term.info "eba" ~version:"0.1.0" ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
