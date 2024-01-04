
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

  
let mi  = {name = "mi";  wins=83; losses=71; total_left=8; left_mi=0; left_csk=1; left_kkr=6;left_dc=1};;
let csk = {name = "csk"; wins=80; losses=79; total_left=3; left_mi=1; left_csk=0; left_kkr=0;left_dc=2};;
let kkr = {name = "kkr"; wins=78; losses=78; total_left=6; left_mi=6; left_csk=0; left_kkr=0;left_dc=0};;
let dc  = {name = "dc";  wins=77; losses=82; total_left=3; left_mi=1; left_csk=2; left_kkr=0;left_dc=0};;
let team_list = [mi; csk; kkr; dc];;


let other_teams current_team team_list = 
  let rec loop other_teams team_list =
    match team_list with
    |[]-> other_teams
    |a::next -> if (a.name = current_team) then loop other_teams next else loop (a::other_teams) next 
  in 
  loop [] team_list
;;


let find_match_pairs list = 
  let rec loop pairs list = 
    match list with
    |[] -> pairs
    |a::next -> 
        let rec loop2 pairs list2 = 
          match list2 with 
          |[]-> pairs
          |b::next -> 
              if (b.name="csk") then loop2 ([a,b,a.left_csk]::pairs) next else
              if (b.name="mi")  then loop2 ([a,b,a.left_mi ]::pairs) next else 
              if (b.name="kkr") then loop2 ([a,b,a.left_kkr]::pairs) next else 
                loop2 ([a,b,a.left_dc ]::pairs) next 
        in let pairs = loop2 pairs next in
        loop pairs next
  in loop [] list
;;
          
let write_graph file current_team team_list = 
  (*file = already created empty team graph file
    index = index of current team in the list
    team_list = list of all teams *)

  (*Open the team graph file to write in*)
  let ff = Out_channel.open_text file in 
  Out_channel.output_string ff "%% This is a graph.\n\n" ;

  let others = other_teams current_team team_list in 
  let pairs = find_match_pairs others in 


  (*Write source node and target node*)
  Out_channel.output_string ff "n 20 300 0\n" ;
  Out_channel.output_string ff "n 300 300 50\n " ;
  (*
  let rec loop_pair_nodes pairs = 
    match pairs with 
   |[] -> Out_channel.output_string ff  "\n";
   |(a,b,v)::next -> 
      Out_channel.output_string ff "n 50 200 %s\n " (a.name^"-"^b.name) ;
      Out_channel.output_string ff "e 0 %d %d\n " v ;
      loop_pair_nodes next 
  in loop_pair_nodes pairs ;
  *)

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

