open Graph


type flow_graph = ((int * int) graph * (int * int));;
type weighted_flow_graph = ((int * int * int) graph * (int * int));;

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph
val add_arc_flow: flow_graph -> id -> id -> int -> flow_graph
val add_arc_weight: weighted_flow_graph -> id -> id -> int -> int -> weighted_flow_graph
val to_int_graph: string graph -> int graph
val int_to_flow_graph: int graph -> int -> int -> flow_graph
val to_flow_graph: (int*int) graph -> int -> int -> flow_graph
val graph_of_flowgraph: flow_graph -> (int * int) graph
val start_of_flowgraph: flow_graph -> int
val end_of_flowgraph: flow_graph -> int
val to_weighted_flow_graph : flow_graph -> weighted_flow_graph
val del_weigths : weighted_flow_graph -> flow_graph
val set_weight:  weighted_flow_graph -> int -> int -> int -> weighted_flow_graph
val get_weight: weighted_flow_graph -> int -> int -> int