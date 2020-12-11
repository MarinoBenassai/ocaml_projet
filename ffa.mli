open Tools
open Graph
open Hashtbl

(* runs the ffa algorithm on a given flowgraph with a given path finding algorithm, returns a completed flowgraph and the maximum flow*)
val ffa: flow_graph -> (flow_graph -> int list) -> (flow_graph * int)

(* gives a possible path in a flowgraph from the start to the end node of the graph, ignores edges with bottleneck 0*)
val find_path_dfs: flow_graph -> int list

(* returns the path extracted from a hash table of parent child nodes from a graph traversal algorithm *)
val get_path: 'a -> 'a -> ('a, 'a) t-> 'a list
