open Gfile
open Tools
open Graph
open Ffa
open Maxmin

let list_string l = String.concat ", " (List.map string_of_int l);;

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)
  (*let () = write_file outfile (gmap (add_arc (gmap graph int_of_string) 1 2 15) string_of_int) in*)
  (*let () = export_flowgraph outfile (to_flow_graph (add_arc_flow (graph_of_flowgraph (int_to_flow_graph (to_int_graph graph) 0 4)) 3 1 4) 0 4) in*)
  let fg = (int_to_flow_graph  (to_int_graph graph) 0 5)  in
  let fg_no = add_arc_flow fg 1 5 21 in
  let fg_no_path = add_arc_flow fg_no 4 5 14 in
  let wfg = to_weighted_flow_graph fg in
  (*let () = Printf.printf "%d" (bottleneck (path_dfs fg) (graph_of_flowgraph fg)) in*)
  (*let () = Printf.printf "[%s]\n" (list_string (path_dfs fg_no)); export_flowgraph outfile fg_no in*)
  (*let () = Printf.printf "[%s]" (list_string (diff [1;2;3;4] [1;2;7;8])) in*)
  let wfg = set_weight wfg 0 1 10 in
  let wfg,_,_ = max_min wfg in
  let () = export_weighted_flowgraph "ffa/finalgraph.dot" wfg in


  ()
