
type team =
 {  
    name : string ;
    wins: int ;
    losses: int ;
    total_left: int ;

    left_mi : int;
    left_csk : int;
    left_kkr : int;
    left_dc : int
  }
(*
let mi  = {wins=83; losses=71; total_left=8; left_mi=0; left_csk=1; left_kkr=6; left_dc=1};;
let csk = {wins=80; losses=79; total_left=3; left_mi=1; left_csk=0; left_kkr=0; left_dc=2};;
let kkr = {wins=78; losses=78; total_left=6; left_mi=6; left_csk=0; left_kkr=0; left_dc=0};;
let dc  = {wins=77; losses=82; total_left=3; left_mi=1; left_csk=2; left_kkr=0; left_dc=0};;

let list_teams = [mi,csk,kkr,dc];;*)

(*Index of teams:
MI  = 1
CSK = 2
KKR = 3
DC  = 4
*)

let write_graph file = 
  let ff = Out_channel.open_text file in 
  Out_channel.output_string ff "%% This is a graph.\n\n" ;

  (*Write source node and target node*)
  Out_channel.output_string ff "n 20 300 0\n" ;
  Out_channel.output_string ff "n 300 300 50\n " ;

  (* Write all nodes (with fake coordinates) 
  n_iter_sorted graph (fun id -> fprintf ff "n %d %d %d\n" (compute_x id) (compute_y id) id) ;
  fprintf ff "\n" ;*)


  (* Write all arcs 
  let _ = e_fold graph (fun count arc -> fprintf ff "e %d %d %d %s\n" arc.src arc.tgt count arc.lbl ; count + 1) 0 in*)

  Out_channel.output_string ff "\n%% End of graph\n" ;


  Out_channel.flush ff;
  Out_channel.close ff;
  ()
;;

let read_teams file =
  let in_ch = open_in file in
    let rec readline teams_list =
      let line = try input_line in_ch with End_of_file -> exit 0
      in
      match line with
      |"end"->teams_list
      |_ -> begin   
        let attribute_list = String.split_on_char ',' line 
        in
          (*print_endline line;*) (*ligne d'affichage de la ligne lue*)

          let new_team = { 
            name = List.nth attribute_list 0;
            wins = int_of_string (List.nth attribute_list 1); 
            losses = int_of_string (List.nth attribute_list 2);
            total_left = int_of_string (List.nth attribute_list 3); 
            left_mi = int_of_string (List.nth attribute_list 4);
            left_csk = int_of_string (List.nth attribute_list 5);
            left_kkr = int_of_string (List.nth attribute_list 6);
            left_dc = int_of_string (List.nth attribute_list 7);
          } in
            readline (new_team::teams_list); 
      end
    in readline []
;;

let rec print_score s = 
  match s with
 |[]-> ()
 |x::li -> Printf.printf "%s, %d, %d, %d, %d, %d, %d, %d\n%!" x.name x.wins x.losses x.total_left x.left_mi x.left_csk x.left_kkr x.left_dc; print_score li;;
(*
let read_lines file  =
  let in_ch = open_in file in
  let rec read_line () =
    let line = try input_line in_ch with End_of_file -> exit 0
    in 
       print_endline line;
       read_line ();
  in read_line ();;
*)




let read_text_file file = 
  let ic = open_in file in
  try
    let line = input_line ic in 
    print_endline line;
    close_in ic;
  with e ->
    close_in_noerr ic;
    raise e;
  ;
;;

