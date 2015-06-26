
open Batteries

open Type

(* A computation step: either the execution of an statement;
 * the evaluation of a branch condition; or the return of
 * a function (possibly) with an expression.
 *)
type step = Stmt of Cil.instr list * Cil.location
          | Test of Cil.exp        * Cil.location
          | Ret  of Cil.exp option * Cil.location

val loc_of_step : step -> Cil.location

(** If condition. *)
type cond = Cond of Cil.exp * Cil.location

(* Execution paths.
 *
 * NB:
 * - Every path ends with [Nil].
 * - Path conditions are encoded with [Assume].
 *)
type t = Nil
       | Assume of cond * bool * t Lazy.t
       | Seq of step * Effects.t * t Lazy.t
       | If of t Lazy.t * t Lazy.t

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
	(Cil.location * path * t Lazy.t) LazyList.t
