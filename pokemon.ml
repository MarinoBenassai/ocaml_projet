open Gfile
open Tools
open Graph
open Ffa
open Csv

let offset = 1000;;

let node_from_csv_line offset graph csv_line = 
  let id = int_of_string (List.hd csv_line) in
  Printf.printf "id: %d offset %d\n" id offset;
  new_node graph (id+offset);;

let new_arc_from_source g csv_line = 
  let id = (int_of_string (List.hd csv_line)) + offset in
  new_arc g (-1) id 1;;

let new_arc_to_sink g csv_line = 
  let id = int_of_string (List.hd csv_line) in
  new_arc g id (-2) 1;;

let match_trainer_pokemon trainer pokemon = 
  (* this is slightly hacky but it takes the 2nd, 3rd and 4th element of the list*)
  let trainer_prefered_types = [(List.hd (List.tl trainer)); (List.hd (List.tl (List.tl trainer)));(List.hd (List.tl (List.tl (List.tl trainer))))] in
  (* this is also slightly hacky buch it checks if the pokemons first or second type is in the trainers prefered types *)
  (List.mem (List.hd (List.tl (List.tl pokemon))) trainer_prefered_types) || 
  (List.mem (List.hd (List.tl (List.tl (List.tl pokemon)))) trainer_prefered_types);;

let csv_line_to_node_id is_trainer line =
  let off = if is_trainer then offset else 0 in
  (int_of_string (List.hd line)) + off;;

let add_arcs_to_one_trainer pokemon graph trainer  = 
  let filtered_pokemon = List.filter (match_trainer_pokemon trainer) pokemon in
  let filtered_pokemon_id = List.map (csv_line_to_node_id false) filtered_pokemon in
  let trainer_id = csv_line_to_node_id true trainer in 
  List.fold_left (fun graph to_node -> new_arc graph trainer_id to_node 1) graph filtered_pokemon_id

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 4 then
    begin
      Printf.printf "\nUsage: %s pokemon trainers outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile_pokemon = Sys.argv.(1)
  and infile_trainers = Sys.argv.(2)
  and outfile = Sys.argv.(3)  in
  (* Read available pokemon *)
  let pokemon_csv = Csv.load_in (open_in infile_pokemon) 
  (* read available trainers *)
  and trainers_csv = Csv.load_in (open_in infile_trainers) in
  (* create source and sink *)
  let source = -1 and sink = -2 in
  let g_source_sink = new_node (new_node empty_graph source) sink in
  (* create nodes for each pokemon *)
  let g_with_pokemon = List.fold_left (node_from_csv_line 0) g_source_sink pokemon_csv in 
  (* create nodes for each trainer *)
  let g_with_trainers = List.fold_left (node_from_csv_line offset) g_with_pokemon trainers_csv in 
  (* create edges from pokemon to sink *)
  let g_pokemon_connected = List.fold_left new_arc_to_sink g_with_trainers pokemon_csv in
  (* create edges from source to trainers *)
  let g_trainers_connected = List.fold_left new_arc_from_source g_pokemon_connected trainers_csv in
  (* create edges for each trainer to each matching pokemon *)
  let g = List.fold_left (add_arcs_to_one_trainer pokemon_csv) g_trainers_connected trainers_csv in
  let fg = (int_to_flow_graph g source sink) in
  let (fg_solved,flow) = (ffa fg find_path_dfs) in
  let () = export_flowgraph outfile fg_solved in
  ()
