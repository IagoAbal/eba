
open Batteries
open Cmdliner

let infer_file fn =
	let file = Frontc.parse fn () in
	Infer.of_file file

let infer_files = List.iter infer_file

(* CLI *)

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE")

let cmd =
	let doc = "Effect-based analysis of C programs" in
	let man = [ `S "DESCRIPTION"; `P "Author: Iago Abal <mail@iagoabal.eu>.";] in
	Term.(pure infer_files $ files),
	Term.info "eba" ~version:"0.1.0" ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
