(** Searchs for paths double-acquiring locks. *)

open Batteries

open Type

type report = {
	fn : Cil.varinfo;
	region : Region.t;
	loc1 : Cil.location;
	loc2 : Cil.location;
	trace : PathTree.path;
}

val pp_report : report -> PP.doc

val in_fundec : Cil.fundec -> FunAbs.t -> report LazyList.t

val in_file : Cil.file -> FileAbs.t -> report LazyList.t
