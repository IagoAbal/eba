
(* TODO: Create immutable wrapper *)

open Batteries

open Type

(** Mapping of variables to shapes, and locations to effects.
 *
 * We track the effects of key location in a C file,
 * those corresponding to C statements.
 *)
type t

val create : Cil.fundec -> t

val fundec : t -> Cil.fundec

val add_var : t -> Cil.varinfo -> shape scheme -> unit

val add_vars : t -> (Cil.varinfo * shape scheme) list -> unit

val find_var : t -> Cil.varinfo -> shape option

val add_loc : t -> Cil.location -> Effects.t -> unit

val add_call : t -> Cil.location -> TypeArgs.t -> unit

val fv_of_locals : t -> Vars.t

val fv_of : t -> Vars.t

val vsubst : Subst.t -> t -> t

val zonk : t -> unit

val finalize : t -> unit

val shape_of : t -> Cil.varinfo -> shape scheme

val regions_of : t -> Cil.varinfo -> Regions.t

val regions_of_list : t -> Cil.varinfo list -> Regions.t

val effect_of : t -> Cil.location -> Effects.t

val args_of_call : t -> Cil.location -> TypeArgs.t

val uninit_locals : t -> Cil.fundec -> Regions.t

(* TODO: This should be precomputed for an immutable FunAbs.t *)
val sum : t -> Effects.t

(** Print to stdout *)
val print : t -> unit

(** Print to stderr *)
val eprint : t -> unit

val fprint : unit IO.output -> t -> unit
