
open Batteries

open Type

type t

type entry = Var of shape scheme * Effects.t
		   | Fun of shape scheme * FunAbs.t

val create : no_globals:int -> t

val add_var : t -> Cil.varinfo -> shape scheme -> Effects.t -> unit

val add_fun : t -> Cil.varinfo -> shape scheme -> FunAbs.t -> unit

val find : t -> Cil.varinfo -> entry

val find_fun : t -> Cil.varinfo -> shape scheme * FunAbs.t

val gvar_regions : t -> Regions.t * Effects.t

val shape_of : t -> Cil.varinfo -> shape scheme

val effect_of : t -> Cil.varinfo -> Effects.t

val sum : t -> Effects.t

val finalize : t -> unit

(** Print to stdout *)
val print : t -> unit

(** Print to stderr *)
val eprint : t -> unit

val fprint : unit IO.output -> t -> unit
