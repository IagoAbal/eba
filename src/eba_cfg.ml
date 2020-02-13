open Batteries
open Cmdliner

open Abs

module L = LazyList

module S = Set


type checks = {
	  chk_uninit : bool
	; chk_dlock  : bool
	; chk_uaf    : bool
	; chk_birq   : bool
}

let run_checks checks file fileAbs :unit =
Printf.printf "This is run_checks";
	let run_check_fun fd in_func =
		let with_warn_out print =
			if Opts.Get.warn_output()
			then File.with_file_out (Cil.(file.fileName) ^ ".warn") print
			else print IO.stdout
		in
		in_func fileAbs fd |> L.iter (fun errmsg ->
		 	with_warn_out (fun out ->
				Printf.fprintf out "\nPotential BUG found:\n%s\n\n" errmsg
			)
		)
	in
	let fds = Cil.(file.globals) |> List.filter_map (function
		| Cil.GFun(fd,_) -> Some fd
		| ______________ -> None
	)
	in
	(* THINK: Too much if-ery? *)
	fds |> List.iter (fun fd ->
		Log.debug "Analyzing function %s\n" Cil.(fd.svar.vname);
		if checks.chk_uninit
		then run_check_fun fd CheckUninitFlow1.in_func;
		if checks.chk_uaf
		then run_check_fun fd CheckUAF.in_func;
		if checks.chk_dlock
		then run_check_fun fd CheckDLockFlow2.in_func;
		if checks.chk_birq
		then run_check_fun fd CheckBhOnIrqFlow2.in_func;
	)

                  (******** EBA-CIL CFG **********)

(******* CFG generator to include regions and shapes per instruction *******)
(* Basic cfg node, it includes the id of the basic block and a forward and backward adjacency list *)
type eba_cfg_node =
  {
    id : int list;
    predc : int list;
    succ : int list;
  }
(*A CIL statement and its labeling in shapes & effects & regions *)
type eba_cfg_stmtreg =
  {s : Cil.stmt list; (* The statement *)
   regs : Cil.stmt list; (*The s&e&r*)
  }

(* The data inside each basic block *)
type eba_cfg_data =
  {ident : int;
   stmts : Cil.stmt * Cil.stmt; (*Instructions inside the basic block*)
  }

type eba_cfg =
  {
    (* The CFG as an adjacency list *)
    cfg : eba_cfg_node list;
    (* The data of each node in the CFG *)
    data : eba_cfg_data list;
  }

(******************************** CFG Creation *******************************)
let create_cfg (fd:Cil.fundec) =
  let basic_cfg = List.map (fun (stmt:Cil.stmt) ->
                      let sid_proj (x:Cil.stmt) = x.sid in
                      {id = stmt.sid::[]; predc = List.map sid_proj stmt.preds;
                       succ = List.map sid_proj stmt.succs}) fd.sallstmts in

  let eq_ x y = x=y in
  let rec find_node n_id ccfg  = match ccfg with
          | hd::xs -> begin
              if List.exists (eq_ n_id) hd.id then hd else find_node n_id xs
            end
          | [] -> assert false (*Should not happen *) in
  (* Merge the cfg nodes *)
  let rec merge_cfg (cfg:eba_cfg_node list) =
    let branch_nodes = List.map (fun n -> List.hd n.id)
                         (List.filter (fun (node:eba_cfg_node ) -> (List.length node.succ) > 1) cfg) in
    (**** Mergable nodes fulfill:
          1. They have only one predecessor.
          2. They have only one successor.
          3. The predecessor is not a branch node *****)
    (**** Fix point merging ****)
    try let mergable = List.find (fun (node:eba_cfg_node) ->
                           (List.length node.predc) = 1 &&
                             (List.length node.succ) = 1 &&
                               not (List.exists (eq_ (List.hd node.predc)) branch_nodes))cfg in
        let pred_node = find_node (List.hd mergable.predc) cfg in
        let new_node = {id = List.append pred_node.id mergable.id; predc = pred_node.predc;
                        succ = ((List.hd mergable.succ) ::
                                  (List.filter (fun n -> not (n = List.hd mergable.id)) pred_node.succ))} in
        let new_cfg = new_node :: (List.filter (fun (node:eba_cfg_node) ->
                                       not (List.exists (fun n -> (n = List.hd mergable.id) || (n = List.hd pred_node.id))
                                              node.id)) cfg) in
        merge_cfg new_cfg
    with Not_found -> cfg in
  (* Collapse long names *)
  let collapse_names (cfg:eba_cfg_node list) =
    (* Main re-label of CFG nodes but keep the original tag list so we can then add the relevant instructions from CIL nodes *)
    let dictionary = List.mapi (fun i (tags,_) -> (tags,i))
                       (List.sort (fun (_,id1) (_,id2) -> if id1 < id2 then -1 else if id1 > id2 then 1 else 0)
                          (List.map (fun (node:eba_cfg_node) -> (node.id, List.hd node.id)) cfg)) in
    (* Lookup final tag, aux function *)
    let rec find_tag dictionary id = match dictionary with
      | (merged, label)::xs -> if List.exists (fun merged_id -> merged_id = id) merged then label else find_tag xs id
      | [] -> assert false (*Should not happen*) in
    (* Actual re-labeling *)
    (dictionary, List.map (fun (node:eba_cfg_node) -> {id = [find_tag dictionary (List.hd node.id)];
                                                       predc = List.map (find_tag dictionary) node.predc;
                                                       succ = List.map (find_tag dictionary) node.succ;}) cfg )in
  let rec fill_cil dictionary = match dictionary with
    |(tags, i)::xs -> (i,List.map (fun nid -> List.find (fun (stmt:Cil.stmt) -> stmt.sid = nid)
                                                fd.sallstmts) tags)::fill_cil xs
    |[] -> [] in

  (*let rec fill_eba = List.map (fun stmt -> AFun.shape_of eba_fd stmt) fd.slocals  in*)

  let (dictionary, graph) = collapse_names (merge_cfg basic_cfg) in
  (graph, fill_cil dictionary)

(******************************** End: CFG Creation *******************************)

(******************************** Dictionary tools *********************************)

(**** Returns all the statements for a given basic block index ****)
let statements_at_index stmts index =
  try List.find (fun (i,a) -> if i = index then true else false) stmts
  with Not_found -> raise Not_found

(**** Returns all the statements inside a given trace *****)
let rec statements_for_trace trace (statements:(int * Cil.stmt list) list) =
  List.concat (List.map (fun bb_id -> let (_,stmts) = statements_at_index statements bb_id in stmts) trace)

(******************************** Dictionary tools *********************************)

(******************************** List tools *********************************)

(* Inits a list with len copies of elem *)
let rec init_list len elem aux = match len with
  | 0 -> aux
  | n -> init_list (n-1) elem (elem::aux)

(* Clones a list having 'n' copies of each element, preserving the order, i.e.
 if the element 'a' is first in the list, the copies of 'a' will be first in
the result*)
let rec clone_list n list aux = match list with
  | hd::rs -> clone_list n rs (List.append (init_list n hd []) aux)
  | [] -> aux


(******************************** List tools *********************************)


let rec nodes_dot_code (rootn : eba_cfg_node list) =
  match rootn with
  |hd::xs -> (List.fold_left (fun prev n -> prev ^ "{\"source\": " ^ string_of_int (List.hd hd.id) ^ ", \"target\": " ^
                                              (string_of_int n) ^ "},\n") "" hd.succ) ^ (nodes_dot_code xs)
  |[] -> "{\"source\":-1, \"target\":-1}\n"



(* String of statements, returns a string for every statemet in the stmts list *)
let rec string_of_stmts stmts eba_fun =
  (*String representation of an expression *)
  let rec string_of_exp (exp:Cil.exp) = match exp with
    | Const _ -> "Constant"
    | Lval a -> string_of_set a
    | AddrOf adr -> (string_of_set adr)
    | _ -> "unknown"
  (*String representation of a variable/pointer set (lval) *)
  and string_of_set (set:Cil.lval) =
    match set with
    | (Mem m, _) -> "Pointer: " ^ string_of_exp m
    | (Var a, _) -> try a.vname ^ Type.Regions.to_string (AFun.regions_of eba_fun a)
                    with Not_found -> a.vname
    (*| (Var a, _) -> match (a.vtype:Cil.typ) with
                    |Cil.TFun(_,_,_,_) -> a.vname
                    |_ -> a.vname ^ Type.Regions.to_string (AFun.regions_of eba_fun a)*)
  in
  (* String of an instruction *)
  let string_of_inst (inst:Cil.instr) = match inst with
    | Set (l,e,loc) -> (*"Set: " ^ string_of_set l ^*) "SET " ^ string_of_int loc.byte ^ Type.Effects.to_string (AFun.effect_of_instr eba_fun loc )
    | Call (l, e, args,loc) -> (*"Call: " ^ string_of_exp e ^*) "CALL " ^ string_of_int loc.byte ^ Type.Effects.to_string (AFun.effect_of_instr eba_fun loc )
    | Asm _ -> (*"ASM"*) "ASM" in
(* String of an statement, returns a string representation of stmt based on its type *)
  let string_of_stmt (stmt:Cil.stmt) = match stmt.skind with
    | If (e, _, _, l)  -> (*"if " ^ string_of_exp e*) (*"if" ^*) Type.Effects.to_string (AFun.effect_of_expr eba_fun l)
    | Loop _ -> (*"loop"*) ""
    | Break _ -> (*"break"*) ""
    | Continue _ -> (*"continue"*) ""
    | Goto _ | ComputedGoto _ -> (*"goto"*) ""
    | Instr i -> List.fold_left (^) "INST" (List.map (fun i -> "\t"^ string_of_inst i ^ "\n") i )
    | Switch _ -> (*"switch"*) ""
    | Block _ -> (*"block"*) ""
    | Return _ -> (*"return"*) ""
    | TryExcept _ -> (*"try-except"*) ""
    | TryFinally _ -> (*"try-finally"*) "" in

  match stmts with
  |(i,stmts)::sx -> string_of_int i ^ " -> " ^
                      List.fold_left (fun prev stmt -> prev ^ (string_of_stmt stmt) ^ "\n")
                        "" stmts ^ string_of_stmts sx eba_fun
  | [] -> ""


(******************** CFG selection ********************)


(**** BFS CFG Node ****)
type bfs_cfg_node =
  {
    eba_node :eba_cfg_node;
    mutable visited : int;
  }

(* Comparition between two bfs-ready nodes, used in sorting the bfs-ready cfg *)
let bfs_cmp bfs1 bfs2 = if bfs1.eba_node.id < bfs2.eba_node.id then (-1) else
                          if bfs1.eba_node.id > bfs2.eba_node.id then 1 else 0
(* Returns a bfs ready cfg from an original cfg *)
let bfs_from_cfg graph = List.sort bfs_cmp (List.map (fun cfg_node -> {eba_node = cfg_node; visited = 0}) graph)

(* Finds a node in a bfs-ready cfg *)
let node bfs (node:int) = List.filter (fun bfs_node -> if List.hd (bfs_node.eba_node.id) = node then true else false)
                            bfs |> List.hd

(******** Returns a sub graph (sub-CFG) from a giving node source (s) to a target node (t) ********)
let sub_cfg cfg source sink =
  (* Returns a forward direction *)
  let dir_forward bfs_node = bfs_node.eba_node.succ in
  (* Returns a backward direction *)
  let dir_backward bfs_node = bfs_node.eba_node.predc in
  (* Updates the bfs mark of the nodes *)
  let rec  update_nodes nodes = match nodes with
      [] -> []
    | hd :: xs -> hd.visited <- 1; hd :: update_nodes xs in
  (* BFS following the edges in the given 'direction' returns the BFS spanning subgraph (plus exit nodes numbers)
     It also returns all possible execution traces
     An execution trace is a path of basic blocks representing linear execution
     @stack       The current stack of cfg_nodes
     @direction   The direction in which the exploration is being performed (backwards, forward)
     @bfs_graph   The bfs tree
     @graph       The graph to be explored
     @exec_stack  The stack for the execution traces
     @exec_traces The execution traces
   *)
  let rec bfs (stack:bfs_cfg_node list) direction bfs_graph graph =
    match stack with
    | [] -> bfs_graph
    | curnt :: xs -> curnt.visited <- 2;
                     ( let unexplored = (update_nodes (List.filter (fun node -> if node.visited < 1 then true else false)
                                                         (List.map (fun id -> node graph id) (direction curnt)))) in

        bfs (List.append xs unexplored ) direction (List.append bfs_graph [curnt.eba_node]) graph)in
  let f_graph = bfs_from_cfg cfg in
  let b_graph = bfs_from_cfg cfg in
  let src = node f_graph source in
  let sk = node b_graph sink in
  let forward_bfs = bfs [src] dir_forward [] (bfs_from_cfg cfg) in
  let backward_bfs = bfs [sk] dir_backward [] (bfs_from_cfg cfg) in
  let forward_set = List.fold_right S.add forward_bfs S.empty in
  let backward_set = List.fold_right S.add backward_bfs S.empty in
  let sub_graph = S.elements (S.intersect forward_set backward_set) in
  (*Prune the sub_graph so it only points inside the subgraph *)
  let sub_node node =
    (* filters either successors or predecessors *)
    let filter_connects l = List.filter (fun i -> List.exists (fun j -> (List.hd (j.id)) = i) sub_graph) l in
    {id = node.id; predc = filter_connects node.predc; succ = filter_connects node.succ} in
  List.map sub_node sub_graph

(******** End: Returns a sub graph (sub-CFG) from a giving node source (s) to a target node (t) ********)

(******** Returns all the execution traces within cfg rooted at a given node (source) ********)
type trace_cfg_node =
  {
    t_id : int list;
    mutable predc : int list;
    mutable succ : int list;
  }
let execution_traces cfg source =
  let exec_ready = List.map (fun node -> {t_id = node.id; predc = node.predc; succ = node.succ}) cfg in
  let rec explore stack graph exec_traces =
    match stack with
    |[] -> exec_traces
    |node :: xs -> (
      match node.succ with
                     (*Either we are at a leaf, so we report the stack or we are in a branching/goto target node
                       and continue up *)
      |[] -> (
        let curnt_node = List.find (fun i -> List.hd i.id = List.hd node.t_id) cfg in
        if List.length curnt_node.succ <> 0
        then
          (node.succ <- curnt_node.succ; explore xs graph exec_traces)
        else
          explore xs graph ((List.rev (List.map (fun p -> List.hd p.t_id) stack))::exec_traces)
      )
      |hd::ts ->
        (*Check if hd is a back edge *)
        let back_edge = List.find_opt (fun x -> List.hd x.t_id = hd) stack in
        match back_edge with
        |None -> node.succ <- ts;
                 explore ((List.find (fun n -> List.hd n.t_id = hd) exec_ready)::stack) graph exec_traces

        |_ ->( (* Create a dummy node with no sucessors so it stops in the next iteration *)
          let new_stack = ((List.find (fun n -> List.hd n.t_id = hd) exec_ready)::stack) in
          node.succ <- ts;
          explore stack graph ((List.rev (List.map (fun p -> List.hd p.t_id) new_stack))::exec_traces)
        )

        (*node.succ <- ts;
        explore ((List.find (fun n -> List.hd n.t_id = hd) exec_ready)::stack) graph exec_traces*)
    ) in
  explore [List.find (fun n -> List.hd n.t_id = source) exec_ready] exec_ready []
(******** End: Returns all the execution traces within cfg rooted at a given node (source) ********)


(******** Return all the execution traces between source and target ********)
let bounded_execution_traces cfg source target =
  let traces = execution_traces cfg source in
  (*Filter the traces to keep only the ones that do end at target *)
  let ht = Hashtbl.create (List.length traces) in
  let rec trim_trace aux tr = match tr with
    |hd::xs -> if hd = target then List.rev (hd::aux) else trim_trace (hd::aux) xs
    |[] -> [] in
  let trs = List.filter
              (fun x -> if List.is_empty x then false else true)
              (List.map (trim_trace []) traces) in
  List.iter (fun t ->
      let found = Hashtbl.find_option ht t in
      match found with
      |None -> Hashtbl.add ht t true
      |_ -> ()) trs;
  let (a,_) = List.split (Hashtbl.to_list ht) in
  a

(******** End:Return all the execution traces between source and target ********)






(******** Returns a string of all the effects of all the given traces in basic block form (int list) ********)
let string_of_int_traces traces ebaFun stmts =
  let output_effects trace =
    (*Outputs the effects of all the instructions in all blocks of the trace *)
    let bb_string (bb_num:int) =
      List.fold_left (^) "" (List.map (fun (ind,s) ->
                                 if ind = bb_num then
                                   (let stm_str = (string_of_stmts [(ind,s)] ebaFun) in
                                    if stm_str <> "" then stm_str^"\n" else "")
                                 else "") stmts  ) in
    List.fold_left (^) "" (List.map (fun n -> (bb_string n) ^ "\n\n") trace) in
  List.fold_left (^) ""
    (List.map (fun trace_set ->
         List.fold_left (^) ""
           (List.map (fun trace -> (output_effects trace)^"===============\n\n\n" ) trace_set)) traces)

(******** End: Returns a string of all the effects of all the given traces ********)


(******************** Lock/Unlock filter  ********************)

type lock_call = {
    (*The name of the lock *)
    lock_name : string;
    (* The basick block in which the lock takes place *)
    mutable lock_bb : int;
  }
type unlock_call = {
    lock_name : string;
    mutable lock_bb : int;
  }
type locking_call =
  | Lock of lock_call
  | Unlock of unlock_call
  | None

let rec lock_unlock_calls stmts eba_fun =

  let get_lock_name (args:Cil.exp list) =
    match args with
    | Lval (Var name,_)::_ -> name.vname
    | AddrOf _::[] -> "AddrOf"
    | AddrOfLabel _::[]-> "AddrOfLevel"
    | _ -> "unkown"
  in


  let is_lock_unlock bb_ind (inst:Cil.instr) =
    let lock_calls = ["mutex_lock";"mutex_lock_nested";"mutex_lock_interruptible_nested";
                      "_spin_lock";"_raw_spin_lock";"__raw_spin_trylock";"_raw_read_lock";
                      "_raw_spin_lock_irq";"_raw_spin_lock_irqsave";"_raw_spin_lock_bh";"spin_lock";
                     "spin_lock_irqsave"] in
    let unlock_calls = ["mutex_unlock";"_spin_unlock";"_raw_spin_unlock";"__raw_spin_unlock";
                        "_raw_read_unlock";"__raw_read_unlock";"_raw_spin_unlock_irq";"__raw_spin_unlock_irq";
                        "_raw_spin_unlock_irqrestore";"_raw_spin_unlock_bh";"spin_unlock_irqrestore";"spin_unlock";
                       "spin_unlock_irqrestore"] in
    match inst with
    | Call (l, e, args,_) -> (match e with
                              |Lval (Var name,_) -> (
                                if List.exists (fun lu -> lu = name.vname) lock_calls then
                                  Lock {lock_name = get_lock_name args; lock_bb = bb_ind}
                                else if List.exists (fun lu -> lu = name.vname) unlock_calls then
                                  Unlock {lock_name=get_lock_name args; lock_bb=bb_ind}
                                else
                                  None
                              )
                              |_ -> None)
    | _ -> None in
  let rec find_lock_unlock bb_ind (stmt:Cil.stmt) = match stmt.skind with
    (*| Loop (block, _,_,_) -> List.concat (List.map (find_lock_unlock (bb_ind + 10)) block.bstmts)*)
    | Instr i -> (List.filter (fun k -> if k = None then false else true)
                    (List.map (fun k -> is_lock_unlock bb_ind k) i))
    (*| Block b -> List.concat (List.map (find_lock_unlock (bb_ind+20)) b.bstmts)*)
    |_ -> [] in
  let rec basic_block_stmts block_stmts = match block_stmts with
    |(i,stmts):: xs -> List.concat ( List.map (find_lock_unlock i) stmts) :: basic_block_stmts xs
    | [] -> [] in
  List.filter (fun l -> not (List.is_empty l)) (basic_block_stmts stmts)

(******************** End: Lock/Unlock filter  ********************)


type function_query_info =
  {
    name : string;
    filename : string;
    cfg : eba_cfg_node list;
    stmts : (int * Cil.stmt list) list;
    eba_fun : Abs.AFun.t;
    mutable lock_unlocks : locking_call list list ;
    mutable full_traces : int list list;
    mutable stmts_per_trace : (Cil.stmt list) list;
  }

(****************************************************************
Process each function in the file and returns a hash table:
name : The name of the function
cfg : The CFG of the function
stmts: The statements/basic block
eba_fun: The Eba's abstraction of the function
 ****************************************************************)
let process_functions cil_file eba_AFuns gcc_filename =
  (*Functions holder table*)
  let function_table = Hashtbl.create 300 in
  Cil.iterGlobals cil_file (fun g ->
      match g with
      | GFun(fd,_) ->
         (let (cfg_root, stmts_list) = create_cfg fd in
          let f = List.find_opt(fun x->(AFun.fundec x).svar.vname=fd.svar.vname) eba_AFuns in
          (*Find eba's function abstraction*)
          match f with
          |Some ebaFun -> Hashtbl.add function_table fd.svar.vname
                               {name=fd.svar.vname; filename = gcc_filename;cfg=cfg_root;
                                stmts=stmts_list;eba_fun=ebaFun;
                                lock_unlocks=[];full_traces = []; stmts_per_trace = []}
          |None -> raise Not_found (*Should never happen*)
         )
      | _ -> ()
    );
  function_table

type query_lastFun =
  {
    mutable last_match: Abs.AFun.t;
  }
(** Outputs the effect and shape of the stmt, eba_fun is the latest funtion that was queried
 It returns the string representation and the function definition that matched **)
let string_of_stmt (stmt:Cil.stmt) last_fun funs_dir =


  let sofie loc is_instr =
    let list_of_funs = List.map (fun (x,y) -> y) (Hashtbl.to_list funs_dir) in
    let rec sofie_aux (funs:function_query_info list) = match funs with
      |hd::rs -> if is_instr then
                   ( try
                       let _ = AFun.effect_of_instr hd.eba_fun loc in
                       last_fun.last_match <- hd.eba_fun;
                       hd.eba_fun
                     with Not_found -> sofie_aux rs
                   )else
                   (
                       try
                         let _ = AFun.effect_of_expr hd.eba_fun loc in
                         last_fun.last_match <- hd.eba_fun;
                         hd.eba_fun
                       with Not_found -> sofie_aux rs
                   )
      |[] -> raise (Not_found) in
    (* Try the last seen function *)
    try
      if is_instr then
        let _ = AFun.effect_of_instr last_fun.last_match loc in
        last_fun.last_match
      else
        let _ = AFun.effect_of_expr last_fun.last_match loc in
        last_fun.last_match
    with Not_found -> sofie_aux list_of_funs in


  let string_of_inst (inst:Cil.instr) = match inst with
    | Set (l,e,loc) ->
       (
         let holding_fun = sofie loc true in
         string_of_int loc.line ^ Type.Effects.to_string (AFun.effect_of_instr holding_fun loc )
       )
    | Call (l, e, args,loc) ->
       (
         let holding_fun = sofie loc true in
         string_of_int loc.line ^ Type.Effects.to_string (AFun.effect_of_instr holding_fun loc )
       )
    | Asm _ -> "" in

  match stmt.skind with
  |Instr [] -> ""
  |Instr i -> List.fold_left (^) "" (List.map (fun i -> "\t"^ string_of_inst i ^ "\n") i )
  |If (e, _, _, l)  ->
    (
      let holding_fun = sofie l false in
      Type.Effects.to_string (AFun.effect_of_expr holding_fun l)
    )
  |_ -> ""


(******** Returns a string of all the effects of all the given traces in statement form (Cil.stmt list) ********)
let string_of_traces traces ebaFun fun_dir fun_name =
  (*List.fold_left (^) "" (List.map (fun trace -> let t_str = string_of_int (List.length trace) in
                         t_str ^ "============\n\n\n") traces)*)
  let lm = {last_match = ebaFun} in
  let filename = fun_name ^ ".txt" in
  let chan = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 filename in
  let trace_2_str trace =
    List.fold_left (^) "" (List.map (fun x -> let s = string_of_stmt x lm fun_dir in
                                              if s = "" then s else s ^ "\n") trace) in
  List.iter (fun trace -> Printf.fprintf chan "%s\n===========\n" (trace_2_str trace)) traces;
  close_out chan

  (*List.fold_left (^) "" (List.map (fun trace -> let t_str =
                           List.fold_left (^) "" (List.map (fun x -> let s = string_of_stmt x lm fun_dir in
                                                                     if s = "" then s else s ^ "\n") trace) in
                         t_str ^ "============\n\n\n") traces)*)

(******** End: Returns a string of all the effects of all the given traces ********)

(**************************************** Functions traces and effects dump ****************************************)
let traces_effects_for_fun fun_name fun_dir =
  let fun_desc = Hashtbl.find fun_dir fun_name in
  if not (List.is_empty fun_desc.cfg) && List.is_empty fun_desc.full_traces then
    (
      fun_desc.full_traces <- execution_traces fun_desc.cfg 0;
      fun_desc.stmts_per_trace <- List.map (fun trace -> statements_for_trace trace fun_desc.stmts)
                                       fun_desc.full_traces;
    ) else ();
  fun_desc.stmts_per_trace




(**************************************** End: Functions traces and effects dump ****************************************)




(****************************************************************
Returns all the effects for a given trace.
*****************************************************************)

let effects_for_trace eba_fun (statements:(int * Cil.stmt list) list) =
  List.map (fun bb_id ->
      let (_,bb_statements) = statements_at_index statements bb_id in
      List.map (fun (s:Cil.stmt) ->
          AFun.effect_of_expr eba_fun (Cil.get_stmtLoc s.skind)) bb_statements)


(****************************************************************
Process a function to calculate its locks and unlocks nodes, its traces
 *****************************************************************)

(********************* Initial 'interesting' functions filter *********************)
let get_interesting_funs funs_dir =
  let filtered = Hashtbl.copy funs_dir in
  Hashtbl.filter_map_inplace (fun fname (data:function_query_info) ->
      if List.length data.cfg < 3 then None
      else
        let lu = lock_unlock_calls data.stmts data.eba_fun in
        if List.length lu <> 0 then
          (
          (*It is an interesting function*)
            data.lock_unlocks <- lu;
            Some data
          )
        else
          None
    ) filtered;
  filtered
(********************* End: Initial 'interesting' functions filter *********************)

(********************* Initial lock/unlock traces queries *********************)

let lock_unlock_queries file_funs =

  (********** Returns yes if inside the list locks_lst there is a locking operation
              that has the same constructor as op **********)
  let rec lock_unlock_finder locks_lst op= match locks_lst, op with
    | (Lock a) as lc ::rs , Lock _ ->lc::lock_unlock_finder rs op
      |(Unlock a) as lc::rs, Unlock _ ->lc::lock_unlock_finder rs op
    |_ :: rs, _ -> lock_unlock_finder rs op
    |[],_ -> [] in

  let rec cross_prod l1 l2 = match l1 with
    | xs::[] -> List.map (fun x -> (xs,x)) l2
    | xs::rs -> List.append (List.map (fun x -> (xs,x)) l2) (cross_prod rs l2)
    | [] -> [] in

  (********** Trace queries generation **********)
  let rec trace_queries (inter_funs:function_query_info list) aux =
    match inter_funs with
    |[] -> aux
    |func :: rs -> let lock_places = List.concat (List.map (fun a ->
                                         lock_unlock_finder a (Lock {lock_name="foo"; lock_bb=0}))
                                       func.lock_unlocks) in
                   let unlock_places = List.concat (List.map (fun a ->
                                         lock_unlock_finder a (Unlock {lock_name="foo"; lock_bb=0}))
                                       func.lock_unlocks) in
                   let possible_queries = cross_prod lock_places unlock_places in
                   (*let queries = List.filter (fun (a,b) -> match a,b with
                                                           |(_,Lock l),(_,Unlock u) -> l.lock_name = u.lock_name
                                                           |_ -> false) possible_queries in *)
                   trace_queries rs ((func, possible_queries)::aux) in
  let (_,intfuns) = List.split (Hashtbl.to_list (get_interesting_funs file_funs)) in
  let queries_list = trace_queries intfuns [] in
  queries_list
(********************* End: Initial lock/unlock traces queries *********************)


(* Scan the trace's statements and per each call, it inlines the code *)
let rec scan_and_inline (trace:Cil.stmt list) funs_dir aux =
  (* Append a list of statments to the aux traces *)
  let append_stmts stmts aux = List.map (fun s ->  List.append s stmts) aux in

  let is_function_call (inst:Cil.instr) =
    match inst with
    |Call(_,Lval (Var a,_),_,loc) -> a.vname
    |_ -> "" in
  let rec remove_calls stmts calls_list =
    match calls_list with
    |c :: rs -> let (_,remaining) = List.partition
                                      (fun inst -> if is_function_call inst = c then true else false)
                                      stmts in
                remove_calls remaining rs
    |[] -> stmts in

  let calls_in_stmt (stmt:Cil.stmt) remove calls_list = match stmt.skind with
    | Instr i -> if not remove then
                   List.filter (fun name -> name <> "") (List.map is_function_call i)
                 else(
                   stmt.skind <- Instr (remove_calls i calls_list);
                   []
                 )
    | _ -> [] in
  let inline_call_stmts fun_name  =
    try let fun_desc = Hashtbl.find funs_dir fun_name in
        if not (List.is_empty fun_desc.cfg) && List.is_empty fun_desc.full_traces then
          (
            (* Calculate the traces and the statements per trace *)
            fun_desc.full_traces <- execution_traces fun_desc.cfg 0;
            fun_desc.stmts_per_trace <- List.map
                                         (fun trace -> statements_for_trace trace fun_desc.stmts)
                                         fun_desc.full_traces
          );
        fun_desc.stmts_per_trace
    with Not_found -> [] in

  let rec process_inline calls curnt_aux =
    let rec cross_traces t aux = match t with
      |a::rs -> let aux_new = List.concat (List.map (fun x -> List.map (fun y -> List.append x y) a) aux) in
                cross_traces rs aux_new
      |[] -> aux in

    let inline_traces = List.filter (fun x -> not (List.is_empty x)) (List.map (inline_call_stmts) calls) in
    (* Cross all inline traces to what we have processed so far *)
    cross_traces inline_traces curnt_aux in


  match trace with
  |hd::rs ->
    (
      let calls = calls_in_stmt hd false [] in
      if List.is_empty calls then
        let new_aux = append_stmts [hd] aux in
        scan_and_inline rs funs_dir new_aux
      else(
        (* We have calls so we inline them *)
        (* First find which functions are 'inlinable', i.e. are NOT defined in the file
           and remove them from the statemt list so we do not inline them again *)
        let (to_remove, _) = List.split (List.filter (fun (_,l) -> l)
                                           (List.combine calls (List.map
                                                                  (fun x -> if List.is_empty (inline_call_stmts x)
                                                                            then false else true) calls))) in
        let _ = calls_in_stmt hd true to_remove in (*side effect *)
        let new_aux = process_inline calls (append_stmts [hd] aux) in
        scan_and_inline rs funs_dir new_aux
      )
    )
  |[] -> List.rev aux


(************** Top call for the inlining processing **************
traces: The traces to be inlined
depth_level: The depth of the inline
funs_dir: The directory of functions in the file
 *******************************************************************)

let rec inline_traces traces depth_level funs_dir =
  match depth_level with
  |0 -> traces
  |n -> if n < 0 then traces else
          (
            let inlined = List.concat (List.map (fun x -> scan_and_inline x funs_dir [[]]) traces) in
            inline_traces inlined (n-1) funs_dir
          )


(** Prototype for checking if a trace is clean or suspicious, interleaved locks and unlocks *)

let rec is_clean_trace list_of_lock_ops trace =

  let rec check_trace (l,u) tr = match tr with
    |[] -> false
    |x::[] -> if x = l || x = u then false else true
    |x::xs -> if x = l || x = u then false else check_trace (l,u) xs in

  match list_of_lock_ops with
  |[] -> true
  |x::rs -> if not (check_trace x trace) then false else is_clean_trace rs trace
(********************* Lock/unlock query processing *********************)
  (*** For each query, for each pair of lock and unlock operation:
       1. Calculate the sub-CFG.
       2. If the sub-CFG is not empty, then calculate all the traces.
       3. For each trace, check there are no other locks and unlocks.
       4. For each trace, request the inline information.
       5. For each trace get the effects string representation.
   ***)
let lock_unlock_qry_proc queries_list funs_dir inline_depth =

  let rec process_function_queries (fun_info, query_list) last_fun =

    let bbs_of_queries = List.map (fun l -> match l with | (Lock x, Unlock y) -> (x.lock_bb,y.lock_bb)|_ -> (-1,-1)) query_list in

    let rec process_function_queries_aux ql =
      match ql with
      |(Lock l,Unlock u)::rs ->
        let subg = sub_cfg fun_info.cfg l.lock_bb u.lock_bb in
        if subg = [] then process_function_queries_aux rs
        else
          (
            let traces = bounded_execution_traces subg l.lock_bb u.lock_bb in
            let other_locks = List.filter (fun (x,y) -> x <> l.lock_bb || y <> u.lock_bb) bbs_of_queries in
            let trimmed_traces = List.map (fun tr -> if List.length tr <= 2 then [] else List.tl(List.rev(List.tl tr))) traces in
            let clean_traces = List.map (is_clean_trace other_locks) trimmed_traces  in
            let to_check = List.combine clean_traces traces in
            List.iter (fun (valid,trace) ->
                match valid with
                | true -> (
                  let stmts_per_trace = statements_for_trace trace fun_info.stmts in
                  let inlined_traces = inline_traces [stmts_per_trace] inline_depth funs_dir in
                  string_of_traces inlined_traces fun_info.eba_fun funs_dir (fun_info.filename ^ "_" ^ fun_info.name);
                );
                | _ -> () ) to_check;
            process_function_queries_aux rs
          )
      |[] -> ()
      |_ -> () in
    process_function_queries_aux query_list in
  (*List.map process_function_queries queries_list*)
  let rec l_u_aux ql = match ql with
    |q::rs ->(
      let (fi,_) = q in
      let (_,ql) = q in

      let lst_fun = {last_match = fi.eba_fun} in
      process_function_queries q lst_fun;
      l_u_aux rs
    )
    |[] -> ()
  in
  l_u_aux queries_list

let full_function_trace funs_dir inline_depth =
  let rec process_function fun_info last_fun =
    let subg = fun_info.cfg in
    if subg = [] then ()
    else
      (
        let traces = execution_traces subg 0 in
        let stmts_per_trace = List.map (fun x -> statements_for_trace x fun_info.stmts) traces in
        let inlined_traces = inline_traces stmts_per_trace inline_depth funs_dir in
          (*let st ="Total number for traces: "^ string_of_int (List.length inlined_traces) ^"\n" ^
          "Total number of stmts: "^ string_of_int (List.fold_left (+) 0 (List.map (List.length) inlined_traces)) in*)

        string_of_traces inlined_traces fun_info.eba_fun funs_dir (fun_info.filename ^ "_" ^ fun_info.name);
      ) in
  (*List.map process_function_queries queries_list*)
  let rec l_u_aux ql = match ql with
    |q::rs ->(
      let (_,fi) = q in
      let lst_fun = {last_match = fi.eba_fun} in
      process_function fi lst_fun;
      l_u_aux rs
    )
    |[] -> ()
  in
  l_u_aux (Hashtbl.to_list funs_dir)


(********************* Lock/unlock query processing *********************)

(**************************************** Query pre-processing ****************************************)
(* Pre-process all the functions for queries *)
let pre_process cil_file eba_file gcc_filename =
  process_functions cil_file eba_file gcc_filename

(************************************* End: Query pre-processing **************************************)


(**************************************** Query entry point ****************************************
 Performs the full query of lock and release it is meant to be called from infer_file or so        *)
let lock_release_query cil_file gcc_filename eba_file =

  (********* Critical region traces generator ************)
  let pre_proc = pre_process cil_file eba_file gcc_filename in
  let uq = lock_unlock_queries pre_proc in
  lock_unlock_qry_proc uq pre_proc 0
  (********* End: Critical region traces generator ************)

  (********* Full traces of functions with locking operations ************)
  (*let pre_proc = get_interesting_funs (pre_process cil_file eba_file gcc_filename) in
  full_function_trace pre_proc 2*)
  (********* End: Full traces of functions with locking operations ************)

  (********* Full traces of all functions ************)

  (*let pre_proc = pre_process cil_file eba_file gcc_filename in
  full_function_trace pre_proc 0*)

  (********* Full traces of all functions ************)


  (*let rec print_list str_list = match str_list with
    |s::rs -> Printf.printf "%s\n" s; print_list rs
    |[] -> ()
  in
  List.iter print_list res*)


(**************************************** End: Query entry point ****************************************)
(* Added by Mahsa *)
class getVarsInGlobalClass1 (pacc: Cil.varinfo list ref) = object
	  inherit Cil.nopCilVisitor
	  method vvrbl (vi: Cil.varinfo) =
			Printf.printf "VD: %s\n"vi.vname;
	    pacc := vi :: !pacc;
	    SkipChildren
		method vvdec (vi: Cil.varinfo) =
			Printf.printf "VD: %s\n"vi.vname;
		  pacc := vi :: !pacc;
		  SkipChildren
	  method vglob = function
	      GType _ | GCompTag _ -> DoChildren
	    | _ -> SkipChildren
	end
	let getVarsInGlobal1 (g : Cil.fundec) : Cil.varinfo list =
	  let pacc : Cil.varinfo list ref = ref [] in
	  let v : Cil.cilVisitor = new getVarsInGlobalClass1 pacc in
	  ignore (Cil.visitCilFunction v g);
	  !pacc
(* End of added by Mahsa *)
let infer_file checks fn =
	let file = Frontc.parse fn () in
	(*Start:Added by Mahsa*)
	let vars_in_file = ref [] in
	    let  fun_in_file= ref [] in
        Cil.iterGlobals file (fun global -> match global with
                                            |Cil.GVarDecl (vinfo,_loc) -> ignore(Errormsg.log "%a %s\n" Cil.d_type vinfo.vtype vinfo.vname); vars_in_file := [vinfo]::!vars_in_file

                                            |Cil.GFun (fundef,_) ->
                                               (*Printf.printf "%s\n" fundef.svar.vname;*)
																							 vars_in_file := (fundef.svar::(getVarsInGlobal1 fundef)) :: !vars_in_file;
																							 fun_in_file:=fundef::!fun_in_file
																						|Cil.GVar (vinfo,_,_) -> (*Printf.printf "%s\n" vinfo.vname;*) vars_in_file := [vinfo]::!vars_in_file;
                                            |_ -> ()
          );
	let vars_flat = List.flatten !vars_in_file in
	let vars = Hashtbl.create (List.length vars_flat) in
	     List.iter(fun (x:Cil.varinfo)-> Hashtbl.add vars x.vname x) vars_flat;

	let fileAbs = Infer.of_file file in
	if Opts.Get.save_abs()
	then begin
		let fn_abs = fn ^ ".abs" in
		File.with_file_out fn_abs (fun out -> AFile.fprint out fileAbs)
	end;
	(* Start: Added by Mahsa *)
	let (out_channe:out_channel) = Pervasives.open_out "/Users/mahv/Desktop/filename.txt" in
	 Errormsg.logChannel := out_channe;
	let temp= List.flatten !vars_in_file in
		 let rec print_list l =
		 match l with
		 |(Cil.Var vi,_o)::xs -> Errormsg.log "%a%s%a\n" Cil.d_type Cil.(vi.vtype) vi.vname; print_list xs
		 |[] -> () in
		 List.iter (fun x -> print_list (List.map (fun vi -> (Cil.Var vi,Cil.NoOffset)) x)) !vars_in_file;
  (*List.iter(Errormsg.log "%a\n" Cil.d_exp) (List.map(fun vi->(Cil.Lval(Var vi,Cil.NoOffset))) temp);*)
	(*A function for printing lists*)
	let aFun_in_file = List.map(fun x->Printf.printf "Afun of:%s\n" Cil.(x.svar.vname);AFile.inst_fun0 fileAbs Cil.(x.svar)) !fun_in_file in
	let shapes_of_arg (a:AFun.t) (v:Cil.varinfo)=
	    AFun.find_var a v in
	List.iter(fun x-> match x with
		|Some y-> AFun.eprint y
		|None->()) aFun_in_file;
	let myUnify_name (a:AFun.t option) (b:AFun.t option)=
	     let rec myUnify_aux1 a1 b1 (x:Cil.varinfo) (xs: Cil.varinfo  list)=
			  	match xs with
					|[]->()
					|y::tail when x.vname=y.vname->
					(*x.vname=y.vname*)
					(*(Type.Shape.of_typ (x.vtype))=(Type.Shape.of_typ (y.vtype))*)
					(*Type.Shape.of_varinfo x= Type.Shape.of_varinfo y*)

					begin
					  Printf.printf "Unification of %s%s with %s%s\n" (AFun.fundec a1).svar.vname x.vname (AFun.fundec b1).svar.vname y.vname ;
						let (arg1, arg2 ) = (AFun.find_var a1 x, AFun.find_var b1 y) in
						match (arg1,arg2) with
						|(Some a1,Some a2) -> Type.Unify.(=~) a1 a2;  Printf.printf "The shapes to be unified are: %s%s\n" (Type.Shape.to_string (Type.Shape.(of_varinfo x)))(Type.Shape.to_string (Type.Shape.(of_varinfo y))); Printf.printf "Unify called\n";
						|_ -> ()
					end
					|_::tail->myUnify_aux1 a1 b1 x tail in
			 match (a,b) with
			 |(Some a1, Some b1)->
			    let aVars=(AFun.fundec a1).sformals in
			    List.iter(fun x-> myUnify_aux1 a1 b1 x (AFun.fundec b1).sformals) aVars
			 |_->() in
	 let list_length=List.length aFun_in_file in
	 let n=0 and m=0 in
   for n=0 to list_length-2 do
	   for m=n+1 to list_length-1 do
       myUnify_name (List.nth aFun_in_file n) (List.nth aFun_in_file m);
    	 match ((List.nth aFun_in_file n),(List.nth aFun_in_file m)) with
    	  |(Some f, Some g) -> AFun.finalize f; AFun.eprint f;
	 											AFun.finalize g; AFun.eprint g
	      |_ ->()
			done
	 done;
	let fn_abs = fn ^ ".abs" in
	File.with_file_out fn_abs (fun out -> AFile.fprint out fileAbs);
	run_checks checks file fileAbs;
	lock_release_query file fn (List.fold_left (fun acc (f:AFun.t option) -> match f with
		|Some (x:AFun.t) -> x::acc
		|_-> acc) [] aFun_in_file);
 				(*lock_release_query file fn fileAbs;*)

  (*Start: Try printing in file*)
	let afun_list= (List.fold_left (fun acc (f:AFun.t option) -> match f with
		|Some (x:AFun.t) -> x::acc
		|_-> acc) [] aFun_in_file) in
	let oc2 = open_out "/Users/mahv/Desktop/filename1.txt" in    (* create or truncate file, return channel *)
	(*List.iter(fun out -> AFile.fprint out oc2) afun_list;*)
  List.iter(fun x->Printf.fprintf oc2 "-------------------------\n"; AFun.fprint oc2 x;) afun_list;  (* write something *)
  close_out oc2;
  (*End: Try printing in file*)

  if Opts.Get.gc_stats()
  then begin
 	 Printf.fprintf stderr "======= GC stats =======\n";
 	 Gc.print_stat stderr;
 	 Printf.fprintf stderr "========================\n"
  end
	(* End: of added by Mahsa *)
	(*End:Added by Mahsa*)

	(*let fileAbs = Infer.of_file file in
	if Opts.Get.save_abs()
	then begin
		let fn_abs = fn ^ ".abs" in
		File.with_file_out fn_abs (fun out -> AFile.fprint out fileAbs);
                (*dump_cfg file fn fileAbs*)
                ()
	end;
	run_checks checks file fileAbs;
        lock_release_query file fn fileAbs;
	if Opts.Get.gc_stats()
	then begin
		Printf.fprintf stderr "======= GC stats =======\n";
		Gc.print_stat stderr;
		Printf.fprintf stderr "========================\n"
	end*)

let infer_file_gcc checks args =
	let fn = Gcc.gcc args in
	infer_file checks fn

(* CLI *)

let log_level_of_int = function
	| x when x <= 0 -> Log.ERROR
	| 1 -> Log.WARN
	| 2 -> Log.INFO
	| x -> Log.DEBUG (* x >= 3 *)

let infer_files verbosity
		flag_gcstats flag_saveabs flag_warn_output flag_fake_gcc
		flag_no_dce flag_no_dfe flag_safe_casts flag_externs_do_nothing
		opt_inline_limit opt_loop_limit opt_branch_limit flag_no_path_check
		flag_all_lock_types flag_no_match_lock_exp flag_ignore_writes
		chk_uninit chk_dlock chk_uaf chk_birq
		files =
	(* CIL: do not print #line directives. *)
	Cil.lineDirectiveStyle := None;
	Log.color_on();
	Log.set_log_level (log_level_of_int verbosity);
	Opts.Set.gc_stats flag_gcstats;
	Opts.Set.save_abs flag_saveabs;
	Opts.Set.warn_output flag_warn_output;
	Opts.Set.dce (not flag_no_dce);
	Opts.Set.dfe (not flag_no_dfe);
	Opts.Set.unsafe_casts (not flag_safe_casts);
	Opts.Set.externs_do_nothing flag_externs_do_nothing;
	Opts.Set.inline_limit opt_inline_limit;
	Opts.Set.loop_limit opt_loop_limit;
	Opts.Set.branch_limit opt_branch_limit;
	Opts.Set.path_check (not flag_no_path_check);
	Opts.Set.all_lock_types flag_all_lock_types;
	Opts.Set.match_lock_exp (not flag_no_match_lock_exp);
	Opts.Set.ignore_writes flag_ignore_writes;
	let checks = { chk_uninit; chk_dlock; chk_uaf; chk_birq } in
	Axioms.load_axioms();
	if flag_fake_gcc
	then infer_file_gcc checks files
	else begin
		List.iter Utils.check_if_file_exists files;
		List.iter (infer_file checks) files
	end

let files = Arg.(non_empty & pos_all string [] & info [] ~docv:"FILE")

(* General *)

(* TODO: Write a Cmdliner.converter for Log.log_level *)
let verbose =
	let doc = "Set the verbosity level." in
	Arg.(value & opt int 0 & info ["v"; "verbose"] ~docv:"LEVEL" ~doc)

let flag_gcstats =
	let doc = "Print GC stats after analyzing a C file." in
	Arg.(value & flag & info ["gc-stats"] ~doc)

let flag_saveabs =
	let doc = "Save effect abstraction to an .abs file." in
	Arg.(value & flag & info ["save-abs"] ~doc)

let flag_warn_output =
	let doc = "Save warns into a .warn file." in
	Arg.(value & flag & info ["warn-output"] ~doc)

let flag_fake_gcc =
	let doc = "Fake GCC and preprocess input file." in
	Arg.(value & flag & info ["fake-gcc"] ~doc)

(* Type inferrer*)

let flag_no_dce =
	let doc = "Do not eliminate dead code." in
	Arg.(value & flag & info ["no-dce"] ~doc)

let flag_no_dfe =
	let doc = "Do not ignore unused fields in structure types (aka dead field elimination)." in
	Arg.(value & flag & info ["no-dfe"] ~doc)

let flag_safe_casts =
	let doc = "Fail on potentially unsafe casts." in
	Arg.(value & flag & info ["safe-casts"] ~doc)

let flag_externs_do_nothing =
	let doc = "Ignore potential side-effects of extern functions." in
	Arg.(value & flag & info ["externs-do-nothing"] ~doc)

(* Model checker *)

let opt_inline_limit =
	let doc = "Inline function calls up to $(docv) times. Provide -1 to prevent inlining but accept some false positives." in
	let def = Opts.Get.inline_limit() in
	Arg.(value & opt int def & info ["inline-limit"] ~docv:"N" ~doc)

let opt_loop_limit =
  let doc = "Take up to $(docv) loop iterations." in
  let def = Opts.Get.loop_limit() in
  Arg.(value & opt int def & info ["loop-limit"] ~docv:"N" ~doc)

let opt_branch_limit =
  let doc = "Take up to $(docv) branch decisions." in
  let def = Opts.Get.branch_limit() in
  Arg.(value & opt int def & info ["branch-limit"] ~docv:"N" ~doc)

let flag_no_path_check =
	let doc = "Do not check path consistency." in
	Arg.(value & flag & info ["no-path-check"] ~doc)

(* Double-Lock bug cheker *)

let flag_all_lock_types =
	let doc = "[Double-Lock] Check all lock types (not only spin locks)." in
	Arg.(value & flag & info ["all-lock-types"] ~doc)

let flag_no_match_lock_exp =
	let doc = "[Double-Lock] Do not use heuristics to match lock object expressions." in
	Arg.(value & flag & info ["no-match-lock-exp"] ~doc)

let flag_ignore_writes =
	let doc = "[Double-Lock] Ignore writes that may affect the lock object." in
	Arg.(value & flag & info ["ignore-writes"] ~doc)

(* Bug chekers *)

let check_uninit =
	let doc = "Check for uses of variables before initialization" in
	Arg.(value & flag & info ["U"; "uninit"] ~doc)

let check_dlock =
	let doc = "Check for double locking" in
	Arg.(value & flag & info ["L"; "dlock"] ~doc)

let check_uaf =
	let doc = "Check for use-after-free" in
	Arg.(value & flag & info ["F"; "uaf"] ~doc)

let check_birq =
	let doc = "Check for BH-enabling while IRQs are off" in
	Arg.(value & flag & info ["B"; "bh-irq"] ~doc)

let cmd =
	let doc = "Effect-based analysis of C programs" in
	let man =
	[
		`S "DESCRIPTION";
		`P "Author: Iago Abal <mail@iagoabal.eu>.";

		`P "To preprocess the input use `--fake-gcc' and pass the necessary arguments after `--', as in:";
		`P "eba --fake-gcc -- -Iinclude/ foo.c";
		`P "EBA will extract the `-D', `-include', and `-I' arguments and invoke GCC, any other option will be ignored."
	] in
	Term.(pure infer_files
		$ verbose
		$ flag_gcstats $ flag_saveabs $ flag_warn_output $ flag_fake_gcc
		$ flag_no_dce $ flag_no_dfe $ flag_safe_casts $ flag_externs_do_nothing
		$ opt_inline_limit $ opt_loop_limit $ opt_branch_limit $ flag_no_path_check
		$ flag_all_lock_types $ flag_no_match_lock_exp $ flag_ignore_writes
		$ check_uninit $ check_dlock $ check_uaf $ check_birq
		$ files),
	Term.info "eba" ~version:"0.1" ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
