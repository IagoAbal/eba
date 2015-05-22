
open Type

type t

type entry = Var of shape scheme * Effects.t
		   | Fun of shape scheme * FunAbs.t

val create : no_globals:int -> t

val add_var : t -> Cil.varinfo -> shape scheme -> Effects.t -> unit

val add_fun : t -> Cil.varinfo -> shape scheme -> FunAbs.t -> unit

val find : t -> Cil.varinfo -> entry

val shape_of : t -> Cil.varinfo -> shape scheme

val effect_of : t -> Cil.varinfo -> Effects.t

val zonk : t -> unit

val pp : t -> PP.doc

val to_string : t -> string
