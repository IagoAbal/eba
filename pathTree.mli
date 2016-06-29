
open Batteries

open Type

(* A computation step: either the execution of an statement;
 * the evaluation of a branch condition; or the return of
 * a function (possibly) with an expression.
 *)
type step = Stmt of Cil.instr list * Cil.location
          | Test of Cil.exp        * Cil.location
          | Goto of Cil.label      * Cil.location (* source *) * Cil.location (* target *)
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
val paths_of : FunAbs.t -> t delayed

type st_pred = Effects.t -> bool

type path = path_entry list

and path_entry =
	(** Decision. *)
	| PEdec of cond * bool (* value *)
	(** Program step. *)
	| PEstep of step * step_kind
	(** Function call.  *)
	| PEcall of Cil.fundec * step * path

and step_kind = SKmatch | SKctx

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
	trace:(Effects.t -> bool) ->
	(step * path * t delayed) LazyList.t

(**
 * Given a program step satistying the target (Q) but not the guard (P),
 * repeatedly performs function inlining, until confirming that there is a path
 * satisfying P EU (P /\ Q).
 *
 * NB: To prevent cycles, inlining is performed up to ~bound times.
 *)
val inline_check :
	bound:int ->
	filter:((step * path * t delayed) LazyList.t -> (step * path * t delayed) LazyList.t) ->
	guard:st_pred -> target:st_pred ->
	trace:(Effects.t -> bool) ->
	file:FileAbs.t -> caller:FunAbs.t ->
	step ->
	(step * path) option

