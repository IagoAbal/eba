
open Batteries

open Type

(** Result expression (if any). *)
type rexp = Rexp of Cil.exp option * Cil.location

(** Statement decomposed into CIL instructions. *)
type stmt = Stmt of Cil.instr list * Cil.location

(** If condition. *)
type cond = Cond of Cil.exp * Cil.location

type t = Nil
       | Return of rexp * Effects.t
       | Seq of stmt * Effects.t * t Lazy.t
       | If of cond * Effects.t * t Lazy.t * t Lazy.t

(* Compute an approximation of all execution paths in a function.
 *
 * A function is interpreted as an automaton; the state of the
 * automaton is formed by an statement and a set of accumulated
 * effects.
 *
 * The algorithm inserts [Nil] and backtracks when an already
 * visited state is reached.
 *)
val paths_of : FunAbs.t -> Cil.fundec -> t Lazy.t

type st_pred = Effects.t -> bool

type path_dec = Dec of cond * bool

type path = path_dec list

val pp_path : path -> PP.doc

(**
 * The reachability engine can model check a function's CFG against
 * queries of the form:
 *
 *   ... when P
 *   Q
 *
 * Which, if I remember correctly, corresponds to `P EU Q' in CTL. Here
 * `P' and `Q' are predicates over computational effects.
 *)
val reachable :
	t Lazy.t ->
	guard:st_pred ->
	target:st_pred ->
	(Cil.location * path) LazyList.t
