open Tools
open Graph
open Hashtbl

val path_dijkstra: weighted_flow_graph -> int list
val get_info: int list -> weighted_flow_graph -> int * int
val max_min: weighted_flow_graph -> (weighted_flow_graph * int * int)
