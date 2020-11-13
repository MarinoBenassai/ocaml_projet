open Graph


type flow_graph = ((int * int) graph * (int * int));;

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph
val add_arc_flow: flow_graph -> id -> id -> int -> flow_graph
val to_int_graph: string graph -> int graph
val int_to_flow_graph: int graph -> int -> int -> flow_graph
val to_flow_graph: (int*int) graph -> int -> int -> flow_graph
val graph_of_flowgraph: flow_graph -> (int * int) graph
val start_of_flowgraph: flow_graph -> int
val end_of_flowgraph: flow_graph -> int