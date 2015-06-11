(** Searchs for uses of uninitialized local variables within functions. *)

open Batteries

open Type

type report = {
	fn : Cil.varinfo;
	x : Cil.varinfo;
	region : Region.t;
	loc : Cil.location;
	trace : PathTree.path;
}

val pp_report : report -> PP.doc

val in_fundec : Cil.fundec -> FunAbs.t -> report LazyList.t

val in_file : Cil.file -> FileAbs.t -> report LazyList.t
