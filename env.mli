
open Batteries

open Type

(** A map from CIL variables to shape schemes *)
type t

val empty : t

val cardinal : t -> int

val find : Cil.varinfo -> t -> shape scheme

val add : Cil.varinfo -> shape scheme -> t -> t

val remove : Cil.varinfo -> t -> t

(** Infix version of [add]. *)
val (+::) : Cil.varinfo * shape scheme -> t -> t

(** Left-biased addition of environments. *)
val (+>) : t -> t -> t

(** If the variable [x] is not bound to a shape, a new environment
	is created where it is given an appropriate generic shape
	(see [Shape.ref_of]).
 *)
val fresh_if_absent : Cil.varinfo -> t -> t

val of_bindings : (Cil.varinfo * shape scheme) list -> t

(** [with_bindings bs env = of_bindings bs +> env] *)
val with_bindings : (Cil.varinfo * shape scheme) list -> t -> t

val zonk : t -> t

val fv_of : t -> Vars.t

(** Print to stdout *)
val print : t -> unit

(** Print to stderr *)
val eprint : t -> unit

val fprint : unit IO.output -> t -> unit
