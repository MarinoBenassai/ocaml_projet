open Graph;;

type flow_graph = ((int * int) graph * (int * int));;

let clone_nodes g = n_fold g new_node empty_graph ;;

(*let change_weight id1 id2 w = *)
let new_arc2 f g id1 id2 w  = new_arc g id1 id2 (f w);;

let gmap g f = e_fold g (new_arc2 f) (clone_nodes g);;

let add_arc g id1 id2 (weight:int) = match find_arc g id1 id2 with
  | Some a -> new_arc g id1 id2 (weight + a)
  | None ->  new_arc g id1 id2 weight;;

let to_int_graph g = gmap g int_of_string;;
let to_flow_graph (g: int graph) (s:int) (e:int) = (gmap g (fun x -> (0, x)), (s, e)) ;;

let graph_of_flowgraph flow_graph = match flow_graph with
  | (a, _) -> a;;

let start_of_flowgraph flow_graph = match flow_graph with
  | (_, (a,_)) -> a;;

let end_of_flowgraph flow_graph = match flow_graph with
  | (_, (_,a)) -> a;;
