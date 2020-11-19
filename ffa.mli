open Tools
open Graph

(*val ffa: flow_graph -> (flow_graph -> int list) -> flow_graph*)
val path_dfs: flow_graph -> int list
val bottleneck: int list -> flow_graph -> int