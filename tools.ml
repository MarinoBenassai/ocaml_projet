let remove_single_arcs ((a:Graph.id),(b:'a Graph.out_arcs)) = (a,[]) 

let clone_nodes (g:'a Graph.graph)  = List.map remove_single_arcs g