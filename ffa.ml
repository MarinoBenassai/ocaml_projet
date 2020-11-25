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
  | x :: y :: tail -> match (find_arc graph x y)  with
      | Some (a,b) -> bottleneck_rec (y::tail) graph (min (b-a) current_bottleneck)
      | None -> failwith "Something is wrong in the path"
  in bottleneck_rec path graph max_int

let rec augment_graph bottleneck path graph = match path with
  | [] -> graph
  | [a] -> graph
  | a :: b :: c -> augment_graph bottleneck (b::c) (add_arc_flow (add_arc_flow graph a b (bottleneck)) b a (-1 * bottleneck))

let choose_arc g id1 id2 w  = match w with
  |(_,0) -> g
  |_,_ -> new_arc g id1 id2 w;;

let clean_graph (g,(s,e))= (e_fold g choose_arc (clone_nodes g), (s,e));;
  
let ffa graph pathfinding =
  let rec ffa_rec graph pathfinding flow =
    let path = pathfinding graph in
    (*check whether it fails*)
    match path with
    | [] -> ((clean_graph graph), flow)
    | p -> let current_bottleneck = bottleneck path graph in
        (*Printf.printf "path: %s with bottleneck %d\n" (list_string p) current_bottleneck;*)
        (*export_flowgraph (Printf.sprintf "ffa/ffa%d.dot" flow) (clean_graph graph);*)
        ffa_rec (augment_graph current_bottleneck p graph) pathfinding (flow+current_bottleneck) in
    ffa_rec graph pathfinding 0;;

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
      
let htable_add htable x y = Hashtbl.add htable y x

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
        List.iter (htable_add htable current_node) successors;
        let new_stack = List.concat [successors;tl] in
        let new_visited = List.concat [successors;visited] in
        find_path_dfs_rec (graph, (start, stop)) new_stack new_visited htable
  in find_path_dfs_rec (graph, (start, stop)) stack visited htable;;
        
