open Graph;;

type flow_graph = ((int * int) graph * (int * int));;
type weighted_flow_graph = ((int * int * int) graph * (int * int));;

let clone_nodes g = n_fold g new_node empty_graph ;;

(*let change_weight id1 id2 w = *)
let new_arc2 f g id1 id2 w  = new_arc g id1 id2 (f w);;

let gmap g f = e_fold g (new_arc2 f) (clone_nodes g);;

let add_arc g id1 id2 (weight:int) = match find_arc g id1 id2 with
  | Some a -> new_arc g id1 id2 (weight + a)
  | None ->  new_arc g id1 id2 weight;;

let add_arc_flow (g, (s, e)) id1 id2 (weight:int) =  match find_arc g id1 id2 with
  | Some (a,b) -> (new_arc g id1 id2 ((weight + a),b), (s, e))
  | None ->  (new_arc g id1 id2 (weight, 0), (s, e));;

let add_arc_weight (g, (s, e)) id1 id2 (value:int) weight =  match find_arc g id1 id2 with
| Some (a,b,c) -> (new_arc g id1 id2 ((value + a),b,c), (s, e))
| None ->  (new_arc g id1 id2 (value, 0, weight), (s, e));;

let to_int_graph g = gmap g int_of_string;;

let int_to_flow_graph (g: int graph) (s:int) (e:int) = (gmap g (fun x -> (0, x)), (s, e)) ;;

let graph_of_flowgraph flow_graph = match flow_graph with
  | (a, _) -> a;;

let start_of_flowgraph flow_graph = match flow_graph with
  | (_, (a,_)) -> a;;

let end_of_flowgraph flow_graph = match flow_graph with
  | (_, (_,a)) -> a;;

let to_weighted_flow_graph (graph, (start, stop)) = (gmap graph (fun (x,y) -> (x,y,0))), (start, stop);;

let get_weight (graph, (start, stop)) id1 id2 = match (find_arc graph id1 id2)  with
| Some (a,b,c) -> c
| None -> failwith "This arc does not exist\n";;
  
let set_weight (graph, (start, stop)) id1 id2 weight = match find_arc graph id1 id2 with
| Some (a,b,c) -> (new_arc graph id1 id2 (a,b,weight), (start, stop))
| None ->  (graph, (start, stop));;

let graph_of_weighted_flowgraph flow_graph = match flow_graph with
  | (a, _) -> a;;

(*Only add an arc if it does not already exist*)
let create_arc graph id1 id2 lbl = match find_arc graph id1 id2 with
  |None -> new_arc graph id1 id2 lbl
  |Some _ -> graph;;