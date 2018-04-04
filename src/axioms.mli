
open Batteries

open Type

val load_axioms : unit -> unit

val find : Cil.varinfo -> shape scheme

val find_partial : Cil.varinfo -> shape -> Effects.t

val footprint :
	Cil.exp
	-> shape
	-> Cil.exp list
	-> shape list
	-> Effects.t
