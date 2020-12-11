open Tools
open Graph
open Hashtbl

(*Returns the shortest path between the start node and the stop node using Dijkstra algorithm.*)
val path_dijkstra: weighted_flow_graph -> int list

(*Returns a solution to the minimum cost maximum flow problem for the weighted flowgraph g, using the Busacker-Gowen algorithm.
After each call, max_min_rec finds the shortest path between the start node and the stop node, and updates the graph
accordingly. The algorithm stops when there is no path left between start and stop. *)
val max_min: weighted_flow_graph -> (weighted_flow_graph * int * int)

