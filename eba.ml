
open Batteries

let find_fd (vn :string) (file :Cil.file) :Cil.fundec =
	let find = function
		| Cil.GFun (fd,_) when Cil.(fd.svar.vname = vn)
		-> Some fd
		| _____other_____
		-> None
	in
	List.find_map find Cil.(file.globals)

let print_fd (fd :Cil.fundec) :unit =
	let fd_doc = Cil.d_global () (Cil.GFun(fd,Cil.builtinLoc)) in
	let fd_str = Pretty.sprint ~width:60 fd_doc in
	output_string stdout fd_str

let _ =
	let file = Frontc.parse "foo.c" () in
    let fd = find_fd "f" file in
	let sch, k = Infer.of_fundec Infer.Env.empty Type.K.none fd in
	print_fd fd;
	output_string stdout "------------------\n";
	Printf.printf "%s\n" Type.(Shape.to_string (sch.body))

