
open Batteries

open Cil

(* THINK: Some of the functions below would not be needed if our front-end, CIL,
 * would keep more information about loops; and if EBA would not rely on
 * `Cil.computeCFGInfo', which simplifies the CFG removing `switch'.
 *)


(** Has `node' a CIL-generated label starting with `prefix' ? *)
let is_labeled_with prefix node :bool =
	node.labels |> List.exists (function
	| Label(n,_,false) -> String.starts_with n prefix
	| x -> false
	)

let is_labeled_while_continue = is_labeled_with "while_continue"

let is_labeled_while_break = is_labeled_with "while_break"

(** Is `node' a goto to a node satisfying `pred' ? *)
let is_goto_with pred node =
	match node.skind with
	| Goto (stmt_ref,_) ->
		pred !stmt_ref
	| _else________________ ->
		false

let is_while_break = is_goto_with is_labeled_while_break

(** Is `node' the entry test of a loop?
 * If so, what is the valuation required to take the loop?
 *
 * is_while_test (if (e) S) = Some true  ---> while (e)  { ... }
 * is_while_test (if (e) S) = Some false ---> while (!e) { ... }
 *)
let is_while_test node : bool option =
	(* Log.info "%d -> %s at %s\n" node.sid (Utils.string_of_cil Cil.d_stmt node) PP.(to_string (Utils.Location.pp (get_stmtLoc node.skind)));
	node.preds |> List.iter (fun node' ->
		Log.info "%d Pred %s\n" node.sid (Utils.string_of_cil Cil.d_stmt node');
		node'.preds |> List.iter (fun node'' ->
			Log.info "%d Pred' %s\n" node.sid (Utils.string_of_cil Cil.d_stmt node'');
		);
	); *)
	match node.skind with
	| If _ ->
		let there_is_while_continue_in = List.exists is_labeled_while_continue in
		let is_suc_of_while_continue =
			there_is_while_continue_in node.preds
		(* or, in case the expression test were side-effectful *)
		|| (if List.is_empty node.preds
			then false
			else there_is_while_continue_in (List.hd node.preds).preds
		    )
		in
		if is_suc_of_while_continue
		then begin
			assert (List.length node.succs = 2);
			if is_while_break (List.nth node.succs 0)
			then Some true  (* while (c)  ---> if (!c) break; *)
			else if is_while_break (List.nth node.succs 1)
			then Some false (* while (!c) ---> if (c) break;  *)
			else None
		end
		else None
	| _else -> Error.panic_with("CilExtra.is_while_test: not an `if' statement")