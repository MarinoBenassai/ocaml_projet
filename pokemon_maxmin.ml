open Gfile
open Tools
open Graph
open Ffa
open Csv
open Maxmin

let offset = 1000;;
let source = -1;;
let sink = -2;;

let node_from_csv_line offset graph csv_line = 
  let id = int_of_string (List.hd csv_line) in
  new_node graph (id+offset);;

let new_arc_from_source g csv_line = 
  let id = (int_of_string (List.hd csv_line)) + offset in
  new_arc g (-1) id 1;;

let new_arc_to_sink g csv_line = 
  let id = int_of_string (List.hd csv_line) in
  new_arc g id (-2) 1;;

let csv_line_to_node_id is_trainer line =
  let off = if is_trainer then offset else 0 in
  (int_of_string (List.hd line)) + off;;

(*Returns true if the pokemon is of the type choosen by the trainer in position rank*)
let match_trainer_pokemon_rank trainer rank pokemon =
  let trainer_prefered_type = List.nth trainer (2 + rank) in
    ((List.nth pokemon 2) = trainer_prefered_type) || 
    ((List.nth pokemon 3) = trainer_prefered_type);;

(*Add the arcs between a trainer node and the nodes of the pokemons compatible, and choose
a weight for each arc orresponding to the trainer's preferences*)    
let add_weighted_arcs_to_one_trainer pokemon graph trainer =
  let rec add_weighted_arcs_to_one_trainer_rec pokemon graph trainer rank =
    let filtered_pokemon = List.filter (match_trainer_pokemon_rank trainer rank) pokemon in
    let filtered_pokemon_id = List.map (csv_line_to_node_id false) filtered_pokemon in
    let trainer_id = csv_line_to_node_id true trainer in 
    let new_graph = List.fold_left (fun graph to_node -> create_arc graph trainer_id to_node (0,1,rank)) graph filtered_pokemon_id in
    if rank == 2 then new_graph
    else add_weighted_arcs_to_one_trainer_rec pokemon new_graph trainer (rank + 1)
  in add_weighted_arcs_to_one_trainer_rec pokemon graph trainer 0;;


let rec get_name_from_id l id = match l with
  | (newid::name::_)::t -> if int_of_string newid = id then name else get_name_from_id t id
  | _::t -> get_name_from_id t id
  | [] -> "";;

(*Takes a solved flow graph, and returns a pairing of each trainer with their pokemon if they have one*)  
let choose_arc pokemon trainers file id1 id2 (a,_,_) = 
  if id1 >= offset && a = 1 then
  ( 
    let selected_trainer = get_name_from_id trainers (id1-offset) in
    let selected_pokemon = get_name_from_id pokemon id2 in
    Printf.fprintf file "Trainer %s with id %d was assigned %s with id %d\n" selected_trainer (id1-offset) selected_pokemon id2
  )
  else if id1 = source && a = 0 then
  let selected_trainer = get_name_from_id trainers (id2-offset) in
    Printf.fprintf file "Trainer %s with id %d was unfortunately not assigned a pokemon\n" selected_trainer (id2-offset)

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s pokemon trainers outfile_graph outfile_results\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile_pokemon = Sys.argv.(1)
  and infile_trainers = Sys.argv.(2)
  and outfile = Sys.argv.(3)  
  and outfile_results = Sys.argv.(4) in
  (* Read available pokemon *)
  let pokemon_csv = Csv.load_in (open_in infile_pokemon) 
  (* read available trainers *)
  and trainers_csv = Csv.load_in (open_in infile_trainers) in
  (* create source and sink *)
  let g_source_sink = new_node (new_node empty_graph source) sink in
  (* create nodes for each pokemon *)
  let g_with_pokemon = List.fold_left (node_from_csv_line 0) g_source_sink pokemon_csv in 
  (* create nodes for each trainer *)
  let g_with_trainers = List.fold_left (node_from_csv_line offset) g_with_pokemon trainers_csv in 
  (* create edges from pokemon to sink *)
  let g_pokemon_connected = List.fold_left new_arc_to_sink g_with_trainers pokemon_csv in
  (* create edges from source to trainers *)
  let g_trainers_connected = List.fold_left new_arc_from_source g_pokemon_connected trainers_csv in
  (* convert from int graph to weighted flow graph *)
  let fg = (int_to_flow_graph g_trainers_connected source sink) in
  let g_weighted = to_weighted_flow_graph fg in
  (* create edges for each trainer to each matching pokemon *)
  let (gr,(start,stop)) = g_weighted in
  let g = List.fold_left (add_weighted_arcs_to_one_trainer pokemon_csv) gr trainers_csv in
  let (fg_solved,flow,cost) = (max_min (g, (start, stop))) in
  let (graph,(_,_)) = fg_solved in
  let results_file = open_out outfile_results in
  let () = Printf.fprintf results_file "%d of %d trainers were assigned a pokemon\n\n" flow (List.length trainers_csv);
           Printf.printf "Results were written to %s and graph to %s\n" outfile_results outfile;
           e_iter graph (choose_arc pokemon_csv trainers_csv results_file) ; 
           export_weighted_flowgraph outfile fg_solved in
  ()
