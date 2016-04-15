
open Batteries

open Type

val find : Cil.varinfo -> shape scheme

val footprint :
	Cil.exp
	-> shape
	-> Cil.exp list
	-> shape list
	-> Effects.t
