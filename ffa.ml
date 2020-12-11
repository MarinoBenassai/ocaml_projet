open Tools;;
open Graph;;
open Gfile;;

(* returns a list of possible successors of node n in graph g *)
let get_viable_successors g n = List.map (fun (a, b) -> a) (List.filter (fun (x, (a,b)) -> a < b) (out_arcs g n))

(* returns the difference between two lists*)
let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1;;

(* return the minimum remaining capacity of the arcs in the path in the flowgraph*)
let bottleneck path (graph, (_, _)) = 
  let rec bottleneck_rec path graph current_bottleneck = match path with
  | [x] -> current_bottleneck
  | [] -> current_bottleneck
  | x :: y :: tail -> match (find_arc graph x y)  with
      | Some (a,b) -> bottleneck_rec (y::tail) graph (min (b-a) current_bottleneck)
      | None -> failwith "Something is wrong in the path"
  in bottleneck_rec path graph max_int

(* augments the path by adding the bottleneck to each arc in the path and the negative bottleneck to the reverse path*)
let rec augment_graph bottleneck path graph = match path with
  | [] -> graph
  | [a] -> graph
  | a :: b :: c -> augment_graph bottleneck (b::c) (add_arc_flow (add_arc_flow graph a b (bottleneck)) b a (-1 * bottleneck))

(* checks if the arc is a "fake" arc and adds it only if it is not *)
let choose_arc g id1 id2 w  = match w with
  |(_,0) -> g
  |_,_ -> new_arc g id1 id2 w;;

(* removes "fake" arcs from graph for nicer visualization *)
let clean_graph (g,(s,e))= (e_fold g choose_arc (clone_nodes g), (s,e));;

(* see ffa.mli file *)
let ffa graph pathfinding =
  let rec ffa_rec graph pathfinding flow =
    let path = pathfinding graph in
    (*check whether it fails*)
    match path with
    | [] -> ((clean_graph graph), flow)
    | p -> let current_bottleneck = bottleneck path graph in
        ffa_rec (augment_graph current_bottleneck p graph) pathfinding (flow+current_bottleneck) in
    ffa_rec graph pathfinding 0;;

(* see ffa.mli file *)
let get_path start stop htable =
  let rec get_path_rec start stop htable path =
    let previous_node = List.hd path in
    let next_node = 
      try Hashtbl.find htable (previous_node)
      with Not_found -> previous_node 
    in if next_node == start then next_node::path
    else if next_node == previous_node then []
    else get_path_rec start stop htable (next_node::path)
  in get_path_rec start stop htable [stop]
      
(* see ffa.mli file *)
let rec find_path_dfs  (graph, (start, stop)) =
  let stack = [start] in
  let visited = [start] in
  let htable = Hashtbl.create 100 in
  let rec find_path_dfs_rec (graph, (start, stop)) stack visited htabl =
    match stack with
      |[] -> []
      |hd::tl -> if hd == stop then get_path start stop htabl
      else let current_node = hd in
        let successors = diff (get_viable_successors graph current_node) visited in
        List.iter (fun x -> Hashtbl.add htable x current_node) successors;
        let new_stack = List.concat [successors;tl] in
        let new_visited = List.concat [successors;visited] in
        find_path_dfs_rec (graph, (start, stop)) new_stack new_visited htable
  in find_path_dfs_rec (graph, (start, stop)) stack visited htable;;
        
