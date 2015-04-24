
(** Unique values *)
type t

(** Thread-safe generator of unique values. *)
val fresh : unit -> t

val compare : t -> t -> int

val to_string : t -> string

(* TODO? module type Unique, implementing uniq_of *)
