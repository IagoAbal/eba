
open Batteries

open Type
open PathTree
open Effects
open Random

module L = LazyList

module AutomataSpec = struct
	type state = 
		| Unlocked
		| Unlocked_final
		| Locked
		| Error of Effects.e

	let transition_labels = [Lock; Unlock] 

	let name = "Double Lock Automata Checker"
	
	type checker_state = {
		current_state: state;
		trace: step list;
		matches: step list;
		kill_region: Regions.t
	}

	let should_permute = true

	let state_to_string state = 
		let open PP in
		let st = match state with 
		| Locked 	-> words "Locked"
        | Unlocked -> words "Unlocked"
        | Unlocked_final -> words "Unlocked_final"
        | Error e -> words "Error" ++ brackets (Effects.pp_e e) in
		st |> PP.to_string

	let checker_state_to_string state = 
		Format.sprintf "{ current_state=%s }" (state_to_string state.current_state)

	let initial_state step func = 
		{ 
			current_state = Unlocked; 
			trace = []; 
			matches = [];
			kill_region = Option.Infix.(
				find_in_stmt CilExtra.find_linux_lock_in_call step >>= Lenv.kregions_of func |? Regions.empty);
		}

	let does_write effects state = 
		Regions.exists (fun r -> (E.(mem (writes ~r) effects))) state.kill_region
	
	let with_previous state _new step = 
		let matches = match _new with | Error _ -> step::state.matches | _ -> state.matches in
		let new_state = { 
			trace=step::state.trace; 
			current_state=_new; 
			matches=matches;
			kill_region=state.kill_region;
		} in
		new_state
				
    let transition previous input step = 
		let next new_state = with_previous previous new_state step in
		let previous_state = previous.current_state in 
		match previous_state with 
		| Unlocked ->
			(match input with 
			| Mem(Lock, _)		-> next Locked
			| _         		-> next previous_state
			)
        | Locked ->
			(match input with 
			| Mem(Lock, _)		-> next (Error input)
			| Mem(Unlock, _)  	-> next Unlocked_final
			)
        | Error _				-> next previous_state
	
	let is_accepting state = 
		match state.current_state with 
		| Error _ 			-> true
		| Unlocked_final 	-> true
		| _ 				-> false

	let is_error state = 
		match state.current_state with 
		| Error _ 			-> true
		| _ 				-> false

	let filter_results (matches: checker_state list) = 
		let sorted = List.sort 
			(fun a b -> Int.compare (List.length a.trace) (List.length b.trace)) 
			matches in 

		let trace_repeats l = match l with
			| [] | [_] -> false
			| (head::tail) -> List.exists (fun t -> Cil.compareLoc head.sloc t.sloc = 0) tail in

		let no_duplicate_regions = List.fold_right (fun current acc -> 
			if List.exists (fun e -> Set.mem e (snd acc)) current.matches || trace_repeats current.trace
			then acc (* If a step has already been detected, skip it. *) 
			else (* Otherwise, include it. *)
				let union = Set.union (Set.of_list current.matches) (snd acc) in
				(current::(fst acc), union))
		sorted ([], Set.empty) in 

		fst no_duplicate_regions

	let pp_checker_state (state:checker_state) = 
		let open PP in 
		let matches = List.rev state.matches in 
		let trace = List.rev state.trace in 
		let match_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline matches in
		let trace_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline trace in
		
		brackets (!^ name) + newline + newline
		++ words "at:" ++ match_locations + newline
		++ words "trace:" ++ trace_locations + newline
end

module Checker = AutomataChecker.Make(AutomataSpec)

include Checker
