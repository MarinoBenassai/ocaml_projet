open Tools;;
open Graph;;
(* find augmenting path*)
(* find bottleneck in path*)
(* augment the graph by adding bottleneck to each arc of the path and subtracting from each opposite arc*)
(* start from top until no augmenting paths may be found *)

let get_viable_successors g n = List.map (fun (a, b) -> a) (List.filter (fun (x, (a,b)) -> a < b) (out_arcs g n))

let path_dfs (graph, (start, stop)) = 
  (*let start = start_of_flowgraph flow_g in
  let end = end_of_flowgraph flow_g in
  let graph = graph_of_flowgraph in*)
  let rec path_dfs_rec visited_nodes node =
    if (not (List.mem node visited_nodes)) && (not (List.mem stop visited_nodes)) then
     begin
      Printf.printf "node is: %d\n" node;
      let successors = get_viable_successors graph node in
      List.fold_left path_dfs_rec (node::visited_nodes) successors
     end
    else visited_nodes

  in path_dfs_rec [] start;;


let bottleneck path graph = 
let rec bottleneck_rec path graph current_bottleneck = match path with
  | [x] -> current_bottleneck
  | [] -> current_bottleneck
  | x :: y :: tail -> match (find_arc graph y x)  with
      | Some (a,b) -> bottleneck_rec (y::tail) graph (min (b-a) current_bottleneck)
      | None -> failwith "Something is wrong in the path"
  in bottleneck_rec path graph max_int
