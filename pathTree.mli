
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

type 'a delayed = unit -> 'a

(* Execution paths.
 *
 * NB:
 * - Every path ends with [Nil].
 * - If there is no [Ret] the path has been cut due to an `exit' or
 *   due to path search bounds.
 * - Path conditions are encoded with [Assume].
 *
 * THINK:
 * - By using [delayed] we avoid having to keep the whole tree in memory.
 * - To avoid constructing it repeteadly, we should combine checkers.
 * - To avoid the extra memory allocation, we could use CPS.
 *)
type t = Nil
       | Assume of cond * bool * t delayed
       | Seq of step * Effects.t * t delayed
       | If of t delayed * t delayed

(* Compute an approximation of all execution paths in a function.
 *
 * A function is interpreted as an automaton; the state of the
 * automaton is formed by an statement and a set of accumulated
 * effects.
 *
 * The algorithm inserts [Nil] and backtracks when an already
 * visited state is reached.
 *)
val paths_of : FunAbs.t -> Cil.fundec -> t delayed

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
	bool -> (* keep searching *)
	t delayed ->
	guard:st_pred ->
	target:st_pred ->
	(Cil.location * path * t delayed) LazyList.t
