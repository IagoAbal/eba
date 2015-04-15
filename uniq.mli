
(** Unique values *)
type t

(** Thread-safe generator of unique values. *)
val fresh : unit -> t

val compare : t -> t -> int

(* TODO? module type Unique, implementing uniq_of *)