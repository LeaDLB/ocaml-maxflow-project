

(*open Ford_fulkerson
  open Gfile
open Tools*)
open Teams
    
let () =
(*
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
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

  let int_graph = gmap graph int_of_string in
  let (new_graph, flow) = (algo_ford_fulkerson int_graph 0 5) in 
  let result_graph = gmap new_graph string_of_int in
  let () = Printf.printf "flow = %d%!\n" flow in

  (* Rewrite the graph that has been read. *)

  let () = write_file outfile result_graph in
  (*let () = export graph "./exportfile.txt"  in*)*)
  let file = "score.txt" in 
  let team_list = read_teams file in   
  let () = Printf.printf " - Equipes extraites du fichier score.txt :\n%!" in
  let () = print_team_list team_list in
  let () = write_graph "./graphs/graphEquipe1.txt" (List.nth team_list 0) team_list in
  let () = write_graph "./graphs/graphEquipe2.txt" (List.nth team_list 1) team_list in
  ()

