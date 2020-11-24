open Tools
open Graph
open Hashtbl


val ffa: flow_graph -> (flow_graph -> int list) -> int -> flow_graph
val path_dfs: flow_graph -> int list
val find_path_dfs: flow_graph -> int list
val get_path: 'a -> 'a -> ('a, 'a) t-> 'a list
val htable_add: ('a, 'b) t -> 'b -> 'a -> unit
val bottleneck: int list -> flow_graph -> int
val diff: 'a list -> 'a list -> 'a list