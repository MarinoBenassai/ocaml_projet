open Tools;;
open Graph;;
open Gfile;;
open Ffa;;

(*We use Ocaml's set as a priority queue, which works as long as we don't need to hold to identical elements in the queue.
Here the queue contains pairs of int: the first int represents the node, the second the distance between that node and start.*)
module PQ_distances = Set.Make
  (struct
    type t = int * int
    let compare (x0, y0) (x1, y1) =
      match Pervasives.compare y0 y1 with
        |0 -> Pervasives.compare x0 x1
        |c -> c
  end);;

(*Returns a list of the nodes that are currently accessible from n, as well as the cost to reach them.*)
let get_viable_successors g n = List.map (fun (a, (x,y,z)) -> (a,z)) (List.filter (fun (x, (a,b,c)) -> a < b) (out_arcs g n))

(*Updates the queue containing the distance of each node to start.*)
let update_distance htable graph current_node current_distance distances (next_node, next_distance) =
  if PQ_distances.exists (fun (x,y) -> x==next_node) distances
  then
  begin
    let _, previous_distance = PQ_distances.choose (PQ_distances.filter (fun (x,y) -> x == next_node) distances) in
      if previous_distance > current_distance + next_distance
      then(
        Hashtbl.add htable next_node current_node;
        PQ_distances.add (next_node, current_distance + next_distance) (PQ_distances.remove (next_node, previous_distance) distances))
      else
        distances
  end
  else
    distances
;;

(*See maxmin.mli*)
let path_dijkstra (graph, (start, stop)) =
  let distances = PQ_distances.empty in
  let distances = (n_fold graph (fun t s -> if s == start then (PQ_distances.add (s,0) t) else (PQ_distances.add (s,max_int) t)) distances) in
  let htable = Hashtbl.create 100 in
    let rec path_dijkstra_rec (graph, (start, stop)) distances htable =
      if PQ_distances.is_empty distances 
      then 
        get_path start stop htable
      else 
        let (current_node, current_distance) = PQ_distances.min_elt distances in
        let distances = PQ_distances.remove (current_node, current_distance) distances in
        let successors = get_viable_successors graph current_node in
        let new_distances = List.fold_left (update_distance htable graph current_node current_distance) distances successors in
        path_dijkstra_rec (graph, (start, stop)) new_distances htable
    in path_dijkstra_rec(graph, (start, stop)) distances htable
    ;;

(*For a given path, returns the minimum capacity of its arcs, as well as the total cost of the path*)
let get_info path (graph, (_, _)) = 
  let rec get_info_rec path graph current_bottleneck current_weight = match path with
  | [x] -> current_bottleneck, current_weight
  | [] -> current_bottleneck, current_weight
  | x :: y :: tail -> match (find_arc graph x y)  with
      | Some (a,b,c) -> get_info_rec (y::tail) graph (min (b-a) current_bottleneck) (current_weight + c)
      | None -> failwith "Something is wrong in the path"
  in get_info_rec path graph max_int 0

(*Updates the values of the flow along the path by adding bottleneck to the current flow of 
each arc, and creating the backward edges*)
let rec update_graph bottleneck path graph = match path with
| [] -> graph
| [a] -> graph
| a :: b :: c -> update_graph bottleneck (b::c) ((add_arc_weight (add_arc_weight graph a b (bottleneck) (get_weight graph a b)) b a (-1 * bottleneck) (-1 * get_weight graph a b)))

(*If the maximum capacity of an arc is 0, do nothing. Else, add the arc back to the graph.*)
let choose_weighted_arc g id1 id2 w  = match w with
  |(_,0,_) -> g
  |_,_,_ -> new_arc g id1 id2 w;;

(*Returns a list of int as a string. Useful for printing paths.*)
  let list_string l = String.concat ", " (List.map string_of_int l);;

(*Delete arcs that were created during the execution of max_min*)
let clean_weighted_graph (g, (s,e)) = (e_fold g choose_weighted_arc (clone_nodes g), (s,e));;


(*See maxmin.mli*)
let max_min graph =
  let rec max_min_rec graph flow cost =
    let path = path_dijkstra graph in
      match path with
        |[] -> (clean_weighted_graph graph), flow, cost
        |p -> let current_bottleneck, current_cost = get_info path graph in
        max_min_rec (update_graph current_bottleneck p graph) (flow+current_bottleneck) (cost + current_cost * current_bottleneck)

  in max_min_rec graph 0 0;