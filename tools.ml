open Graph;;

let clone_nodes g = n_fold g new_node empty_graph ;;

(*let change_weight id1 id2 w = *)
let new_arc2 f g id1 id2 w  = new_arc g id1 id2 (f w);;

let gmap g f = e_fold g (new_arc2 f) (clone_nodes g);;
