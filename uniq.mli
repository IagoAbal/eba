
(** Unique values *)
type t

(** Thread-safe generator of unique values. *)
val fresh : unit -> t

val compare : t -> t -> int

val to_int : t -> int

val pp : t -> PP.doc

val to_string : t -> string
