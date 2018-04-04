open Batteries

let rec filter_cpp_args = function
	| []
	-> []
	| (a::args)
	when String.(starts_with a "-D"
	          || starts_with a "-U"
	          || starts_with a "-I")
	-> a :: filter_cpp_args args
	| ("-include" as a1)::a2::args
	-> a1 :: a2 :: filter_cpp_args args
	| __skip::args
	-> filter_cpp_args args

let find_c_file args =
	try
		let fp = List.find (fun s -> String.ends_with s ".c") args in
		Utils.check_if_file_exists fp;
		fp
	with
		Not_found ->
			Printf.eprintf "eba: no input .c files";
			exit 1

let gcc args =
	(* should use Fpath.of_string *)
	let c_file = Fpath.v @@ find_c_file args in
	let dir, fname = Fpath.split_base c_file in
	let cpp_dir = Fpath.(v "_eba" // dir) in
	(* should check for failure *)
	ignore @@ Sys.command (Printf.sprintf "mkdir -p %s" @@ Fpath.to_string cpp_dir);
	let cpp_args = filter_cpp_args args in
	let cpp_file = Fpath.(cpp_dir // fname) in
	(* should check for failure *)
	ignore @@ Sys.command Fpath.(Printf.sprintf
			"gcc -E -o %s %s %s"
			(to_string cpp_file)
			(String.concat " " cpp_args)
			(to_string c_file));
	Fpath.to_string cpp_file
