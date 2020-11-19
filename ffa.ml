open Tools;;
open Graph;;
(* find augmenting path*)
(* find bottleneck in path*)
(* augment the graph by adding bottleneck to each arc of the path and subtracting from each opposite arc*)
(* start from top until no augmenting paths may be found *)

let get_viable_successors g n = List.map (fun (a, b) -> a) (List.filter (fun (x, (a,b)) -> a < b) (out_arcs g n))

let list_string l = String.concat ", " (List.map string_of_int l);;
let path_dfs (graph, (start, stop)) = 
  (*let start = start_of_flowgraph flow_g in
  let end = end_of_flowgraph flow_g in
  let graph = graph_of_flowgraph in*)
  let rec path_dfs_rec visited_nodes node =
    if (not (List.mem node visited_nodes)) && (not (List.mem stop visited_nodes)) then (* visited nodes = [0, 2] node = *)
     begin
      if (node == stop) then
      begin
        (stop::visited_nodes)
      end
      else
      begin
        Printf.printf "node is: %d  " node;
        let successors = get_viable_successors graph node in
        Printf.printf "Successors: %s\n"  (list_string successors);
        Printf.printf "visited: %s\n"  (list_string visited_nodes);
        if successors == [] then visited_nodes
        else List.fold_left path_dfs_rec (node::visited_nodes) successors
      end
    end
    else visited_nodes

  in match (path_dfs_rec [] start) with
  | [] -> []
  | hd::tl -> if hd == stop then hd::tl else [];; (*return empty list if stop node is not last node*)


let bottleneck path (graph, (_, _)) = 
  let rec bottleneck_rec path graph current_bottleneck = match path with
  | [x] -> current_bottleneck
  | [] -> current_bottleneck
  | x :: y :: tail -> match (find_arc graph y x)  with
      | Some (a,b) -> bottleneck_rec (y::tail) graph (min (b-a) current_bottleneck)
      | None -> failwith "Something is wrong in the path"
  in bottleneck_rec path graph max_int

(*let rec augment_graph bottleneck path graph = match path with
  | [] -> graph
  | [a] -> graph
  | a :: b :: c -> augment_graph bottleneck (b::c) (add_arc_flow (add_arc_flow graph a b bottleneck) b a (-1 * bottleneck))*)
  
(*let rec ffa graph pathfinding = 
  let path = pathfinding graph in
  (*check whether it fails*)
  match path with
  | [] -> Printf.printf "End reached"; graph
  | p -> let current_bottleneck = bottleneck path graph in
      Printf.printf "path: %s with bottleneck %d" list_string bottleneck;
      ffa (augment_graph p graph) pathfinding
    *)
