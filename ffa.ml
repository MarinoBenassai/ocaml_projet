open Tools;;
open Graph;;
open Gfile;;
(* find augmenting path*)
(* find bottleneck in path*)
(* augment the graph by adding bottleneck to each arc of the path and subtracting from each opposite arc*)
(* start from top until no augmenting paths may be found *)

let get_viable_successors g n = List.map (fun (a, b) -> a) (List.filter (fun (x, (a,b)) -> a < b) (out_arcs g n))

let list_string l = String.concat ", " (List.map string_of_int l);;

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1;;

let path_dfs (graph, (start, stop)) =   
  let stack = [start] in
  let visited = [] in
  let rec path_dfs_rec (graph, (start, stop)) stack visited path =
    Printf.printf "stack: [%s]\n" (list_string stack);
    match stack with
      |[] -> []
      |hd::tl -> let current_node = hd in
        Printf.printf "current: %d\n" current_node ; 
        if current_node == stop then ((Printf.printf "stop\n\n"); stop::path)
        else if current_node == -1 then 
          path_dfs_rec (graph, (start, stop)) tl visited (List.tl path)
        else
        let new_visited = current_node::visited in
        let new_path = current_node::path in
        let successors = diff (get_viable_successors graph current_node) new_visited in
        if successors = [] then  path_dfs_rec (graph, (start, stop)) tl new_visited path
        else let new_stack = List.concat [successors;[-1];tl] in
        path_dfs_rec (graph, (start, stop)) new_stack new_visited new_path
  in path_dfs_rec (graph, (start, stop)) stack visited []

let bottleneck path (graph, (_, _)) = 
  let rec bottleneck_rec path graph current_bottleneck = match path with
  | [x] -> current_bottleneck
  | [] -> current_bottleneck
  | x :: y :: tail -> match (find_arc graph y x)  with
      | Some (a,b) -> bottleneck_rec (y::tail) graph (min (b-a) current_bottleneck)
      | None -> failwith "Something is wrong in the path"
  in bottleneck_rec path graph max_int

let rec augment_graph bottleneck path graph = match path with
  | [] -> graph
  | [a] -> graph
  | a :: b :: c -> augment_graph bottleneck (b::c) (add_arc_flow (add_arc_flow graph a b (-1* bottleneck)) b a (bottleneck))
  
let rec ffa graph pathfinding n = 
  let path = pathfinding graph in
  (*check whether it fails*)
  match path with
  | [] -> Printf.printf "End reached"; graph
  | p -> let current_bottleneck = bottleneck path graph in
      Printf.printf "path: %s with bottleneck %d" (list_string p) current_bottleneck;
      export_flowgraph (Printf.sprintf "ffa/ffa%d.dot" n) graph ;
      ffa (augment_graph current_bottleneck p graph) pathfinding (n+1);;
    
