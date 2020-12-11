open Graph

(* flow_graph type contains an (int*int graph)  where the first int is the current flow and the second int is the maximum capacity of an arc, 
   furthermore it contains the ids of the start and end node*)
type flow_graph = ((int * int) graph * (int * int));;

(* similar to flow_graph but contains a third parameter for each arc which is the cost of traversing this arc *)
type weighted_flow_graph = ((int * int * int) graph * (int * int));;

(* returns the graph without the arcs *)
val clone_nodes: 'a graph -> 'b graph

(* applies a function to each arc of a graph and returns the resulting graph *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

(* adds an arc to an int graph, if not already present. otherwise augments the arc by adding the weight *)
val add_arc: int graph -> id -> id -> int -> int graph

(* same as add_arc but for flow graph, the current flow is augmented *)
val add_arc_flow: flow_graph -> id -> id -> int -> flow_graph

(* same as add_arc but for weighted flow graph, the current flow is augmented *)
val add_arc_weight: weighted_flow_graph -> id -> id -> int -> int -> weighted_flow_graph

(* converts a string graph to an int graph using int_to_string for each arc *)
val to_int_graph: string graph -> int graph

(* converts an int graph to a flow graph. the weight is interpreted as the maximum capacity of an arc, the current flow is 0 *)
val int_to_flow_graph: int graph -> int -> int -> flow_graph

(* returns the graph component of a flow_graph *)
val graph_of_flowgraph: flow_graph -> (int * int) graph

(* returns the start  of a flow_graph *)
val start_of_flowgraph: flow_graph -> int

(* returns the graph component of a flow_graph *)
val end_of_flowgraph: flow_graph -> int

(* converts a flow graph to a weighted flow graph, by giving each arc a weight of 0*)
val to_weighted_flow_graph : flow_graph -> weighted_flow_graph

val set_weight:  weighted_flow_graph -> int -> int -> int -> weighted_flow_graph

(* returns the weight associated to an arc in a weighted flow graph*)
val get_weight: weighted_flow_graph -> id -> id -> int

(* add an arc to a graph only if that arc do not already exists*)
val create_arc: 'a graph -> id -> id -> 'a -> 'a graph

(* returns the graph component of a weighted flow_graph *)
val graph_of_weighted_flowgraph: weighted_flow_graph -> (int * int * int) graph