
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
	let file = Frontc.parse "bar.c" () in
	Infer.of_file file
