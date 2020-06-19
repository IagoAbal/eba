open Batteries

module L = LazyList

open Type
open Abs
open PathTree
open Effects
open PathTree

module type AutomataSpec = sig
	(** A name to identify the checker *)
	val name : string
	type state

	(** Checker's internal state, eg. memory regions to track. *)
	type checker_state = {
		current_state: state;
		trace: step list;
		matches: step list;
		kill_region: Regions.t
	}

	(** States *)
	val initial_state : step -> AFun.t -> checker_state
	val is_accepting : checker_state -> bool
	val does_write : effects -> checker_state -> bool
	val should_permute : bool
	val is_error : checker_state -> bool
	val transition_labels : mem_kind list
	val pp_checker_state : checker_state -> SmartPrint.t
	val checker_state_to_string : checker_state -> string
	val filter_results : checker_state list -> checker_state list

	(** Test *)
	val transition : checker_state -> Effects.e -> step -> checker_state
end

module type S = sig
	type result
	val check : AFile.t -> Cil.fundec -> result list
	val filter_results : result list -> result list
	val stringify_results : result list -> string list
end

module Make (A : AutomataSpec) : S = struct
	type result = A.checker_state
	type checking_type = Must | May

	let is_in_transition_labels effect = 
		match effect with 
		| Mem(kind, _) -> List.mem kind A.transition_labels
		| _ -> false 

	let rec permute l = 
		let insert_all_positions x l = 
			let rec aux prev acc l = 
				match l with
				| [] -> (prev @ [x]) :: acc |> List.rev
				| hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl in 
				aux [] [] l in
		match l with 
		| [] -> []
		| [hd] -> [[hd]]
		| hd::tl -> List.fold_left (fun acc p -> acc @ insert_all_positions hd p) [] (permute tl)

	let apply_to_region e success failure = 
		match e with 
		| Mem(_, r) -> success r
		(* 	If the given effect isn't a Mem, then we're not interested in it, since it doesn't contain relevant information. *)
		| _ -> failure

	let for_all_results es map pred = List.for_all (fun e -> 
		apply_to_region e (fun r -> 
			let results = Map.find r map in 
			List.for_all (fun state -> pred state) results
		) false) es

	let for_any_results es map pred = List.exists (fun e -> 
		apply_to_region e (fun r -> 
			let results = Map.find r map in 
			List.exists (fun state -> pred state) results
		) false) es

	let print_map m s = 
		if Map.is_empty m then Format.printf "%s %s" s "Empty\n"
		else
			let gen_names ss = List.fold_right (fun s acc -> Format.sprintf "%s " (A.checker_state_to_string s) ^ acc ) ss "" in
			Format.printf "%s\n" s;
			BatMap.iter (fun k v -> Format.printf "%s: %s" (Region.pp k |> PP.to_string) (v |> gen_names) ) m;
			Format.printf "%s" "\n"

	let stringify_effects effects = 
		List.fold_right (fun e acc -> Format.sprintf "%s %s " (pp_e e |> PP.to_string) acc) effects ""

	let rec explore_paths func path map check_type = 
		let p = path() in
		match p with
		| Seq(step, remaining) -> 
			let input = (match check_type with
				| May 	-> EffectSet.filter is_in_transition_labels step.effs.may
				| Must 	-> EffectSet.filter is_in_transition_labels step.effs.must) 
			|> EffectSet.to_list 
			in
			
			(* Skip step if the effects are uninteresting *)
			if List.is_empty input 
			then explore_paths func remaining map check_type
			else
				let apply_transition effect map_to_add_to = 
					let result = apply_to_region effect (fun r -> 
						(* Find the previous result if present, then determine new checker_state. *)
						let result = Map.find_default [A.initial_state step func] r map_to_add_to in 
						let applied = 
							List.map (fun s -> 
								if A.does_write step.effs s || A.is_accepting s 
								then s 
								else (A.transition s effect step)) result 
						in
						Map.add r applied map_to_add_to) map_to_add_to in
					result
				in

				(* 	Find all permutations of effects e.g. {{lock, unlock} -> {{lock, unlock}, {unlock, lock}} 
					in order to evaluate all effect orders. *)
				let permutations = if A.should_permute then permute input else [input] in 
				let result = List.fold_left (fun map_to_change effects ->
						(* 	For each effect in a given permutation, apply the transition function, 
							and add the result to the (region, checker_state) -> [checker_state] map. 
							
							This expresses that a given region has multiple state machines monitoring it 
							if multiple evaluation orders are possible for the effects of that region. *)
						List.fold_left (fun acc effect -> apply_transition effect acc) map_to_change effects 
					) map permutations in

				let are_all_error = for_all_results input result A.is_error in
				let are_all_not_error = for_all_results input result (fun state -> not (A.is_error state)) in

				let result_killed = 
					Map.filter_map (fun _ value -> 
						let filtered = List.filter (fun state -> not (A.is_accepting state) || A.is_error state) value in
						if List.is_empty filtered then None else Some filtered)
					result in

				(* 	If some --- but not all --- states are errors then there's uncertainty about whether the given step 
					will lead to an error. Therefore we inline in order to find out whether an error is really present. 
					If all states are errors or all states are not errors, we just continue exploration. 
				*)
				if are_all_error || are_all_not_error then explore_paths func remaining result_killed check_type
				else 
					let inlined_result = 
						match inline func step with
						| Some (_, inlined_path) 	-> explore_paths func inlined_path map Must
						| _ 						-> result_killed
					in
					explore_paths func remaining inlined_result check_type
		| Assume(_, _, remaining) -> 
			explore_paths func remaining map check_type
		| If(true_path, false_path) -> 
			let true_branch = explore_paths func true_path map check_type in
			let false_branch = explore_paths func false_path map check_type in
			let merge = 
				Map.merge (fun _ a b -> 
					(match a, b with 
					| Some aa, Some bb -> Some (aa @ bb)
					| None, Some _ -> b
					| Some _, None -> a
					| None, None -> None)
				) true_branch false_branch in 
			merge
		| Nil -> map

	let check file declaration =
		let variable_info = Cil.(declaration.svar) in
		match variable_info.vstorage with 
		| Static -> []
		| _ ->
			let _, global_function = Option.get(AFile.find_fun file variable_info) in
			let path_tree = paths_of global_function in
			let results = explore_paths global_function path_tree Map.empty May in 
			let states = Map.values results in
			let matches = Enum.fold (fun acc m -> (List.filter A.is_error m) @ acc) [] states in
			let matches_reversed = List.rev matches in 
			matches_reversed

	let filter_results matches = A.filter_results matches

	let stringify_results matches = 
		let pp = List.map (fun m -> A.pp_checker_state m) matches in
		let pp_list = List.map (fun m -> PP.to_string m) pp in
		pp_list
end