open Tools
open Graph

(*var ffa: flow_graph -> (flow_graph -> int list) -> flow_graph*)

val path_dfs: flow_graph -> int list
val bottleneck: int list -> (int * int) graph -> int