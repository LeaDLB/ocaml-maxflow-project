open Gfile

type team =
 {  
    id: int;
    name : string ;
    wins: int ;
    losses: int ;
    total_left: int ;

    left_mi : int;
    left_csk : int;
    left_kkr : int;
    left_dc : int
  }

let rec print_team_list l = 
  match l with
  |[]-> ()
  |x::li -> Printf.printf "%d, %s, %d, %d, %d, %d, %d, %d, %d\n%!" x.id x.name x.wins x.losses x.total_left x.left_mi x.left_csk x.left_kkr x.left_dc; 
  print_team_list li;;

let other_teams current_team team_list = 
  let rec loop other_teams team_list =
    match team_list with
    |[]-> List.rev other_teams
    |a::next -> if (a.name = current_team) then loop other_teams next else loop (a::other_teams) next 
  in 
  loop [] team_list
;;


let find_match_pairs list = 
  let rec loop pairs list = 
    match list with
    |[] -> List.rev pairs
    |a::next -> 
        let rec loop2 pairs list2 = 
          match list2 with 
          |[]-> pairs
          |b::next -> 
              if (b.name="csk") then loop2 ((a,b,a.left_csk)::pairs) next else
              if (b.name="mi")  then loop2 ((a,b,a.left_mi )::pairs) next else 
              if (b.name="kkr") then loop2 ((a,b,a.left_kkr)::pairs) next else 
                loop2 ((a,b,a.left_dc )::pairs) next 
        in let pairs = loop2 pairs next in
        loop pairs next
  in loop [] list
;;

let rec write_nodes file length index  = 
if index=length then () 
else
  begin
    let x = compute_x index in 
    let y = compute_y index in
    let string = "n "^(string_of_int x)^" "^(string_of_int y)^" "^(string_of_int index)^"\n" in 
    Out_channel.output_string file string ; write_nodes file length (index+1)
  end 
 ;;

 let rec write_source_arcs file pairs index index_arc= 
  match pairs with 
 |[] -> ()
 |(_,_,v)::next -> 
    Out_channel.output_string file ("e 0 "^(string_of_int (index))^" "^(string_of_int index_arc)^" "^(string_of_int v)^"\n");
    write_source_arcs file next (index+1) (index_arc+1)
 ;;

 let rec write_end_arcs current_team length_pairs length_others file others index_arc = 
  match others with 
 |[] -> ()
 |team::next -> 
    let v = (current_team.wins+current_team.total_left) - team.wins in
    let string = ("e "^(string_of_int team.id)^" "^(string_of_int (length_others+length_pairs+1))^" "^(string_of_int index_arc)^" "^(string_of_int v)^"\n") in
    Out_channel.output_string file string;
    write_end_arcs current_team length_pairs length_others file next (index_arc+1)
 ;;

let rec write_infinite_arcs file length_pairs pairs index index_arc=
  match pairs with 
  |[] -> ()
  |(teamA,teamB,_)::next -> 
    let string1 = "\ne "^(string_of_int (index))^" "^(string_of_int teamA.id)^" "^(string_of_int(index_arc))^" "^(string_of_int max_int) in 
    let string2 = "\ne "^(string_of_int (index))^" "^(string_of_int teamB.id)^" "^(string_of_int(index_arc+1))^" "^(string_of_int max_int) in 
    Out_channel.output_string file string1 ;
    Out_channel.output_string file string2 ;
    write_infinite_arcs file length_pairs next (index+1) (index_arc+2)
;; 







          
let write_graph file current_team team_list = 
  (*Open the team graph file to write in*)
  let ff = Out_channel.open_text file in 
  Out_channel.output_string ff "%% This is a graph.\n\n" ;

  (*Variables*)
  let others = other_teams current_team.name team_list in 
  let pairs = find_match_pairs others in 
  let length_others = List.length others in 
  let length_pairs = List.length pairs in 
  

  (*Write source node*)
  Out_channel.output_string ff "n 20 300 0\n" ;
  
  (*Write nodes*)
  let () = write_nodes ff length_others 1 in
  let () = write_nodes ff length_pairs (length_others+1) in 
  
  (*Write target node*)
  Out_channel.output_string ff ("n 500 300 "^(string_of_int (length_others+length_pairs+1))^"\n\n") ;

  (*
  Out_channel.output_string ff "n 100 200 1\n" ;
  Out_channel.output_string ff "n 100 300 2\n" ;
  Out_channel.output_string ff "n 100 400 3\n" ;

  Out_channel.output_string ff "n 200 200 4\n" ;
  Out_channel.output_string ff "n 200 300 5\n" ;
  Out_channel.output_string ff "n 200 400 6\n" ;*)

  

  (*Write 3 arcs from source*)
  let () = write_source_arcs ff pairs (length_others+1) 0 in

  (*Write infinite arcs*)
  let () = write_infinite_arcs ff length_pairs pairs (length_others+1) (length_pairs+2) in

  (*Write 3 last arcs to the target*)
  let () = write_end_arcs current_team length_pairs length_others ff others (length_pairs*3+1) in 

  (*
  let val1 = (current_team.wins+current_team.total_left) - (List.nth others 0).wins in
  let val2 = (current_team.wins+current_team.total_left) - (List.nth others 1).wins in 
  let val3 = (current_team.wins+current_team.total_left) - (List.nth others 2).wins in
  Out_channel.output_string ff ("e 4 7 3 "^(string_of_int) val1);
  Out_channel.output_string ff ("\ne 5 7 4 "^(string_of_int) val2);
  Out_channel.output_string ff ("\ne 6 7 5 "^(string_of_int) val3);

  (*Write the 6 infinite arc*)
  Out_channel.output_string ff ("\ne 1 4 6 "^(string_of_int) max_int);
  Out_channel.output_string ff ("\ne 1 5 7 "^(string_of_int) max_int);
  Out_channel.output_string ff ("\ne 2 4 8 "^(string_of_int) max_int);
  Out_channel.output_string ff ("\ne 2 6 9 "^(string_of_int) max_int);
  Out_channel.output_string ff ("\ne 3 5 10 "^(string_of_int) max_int);
  Out_channel.output_string ff ("\ne 3 6 11 "^(string_of_int) max_int);*)

  Out_channel.output_string ff "\n\n%% End of graph\n" ;

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
      |"end"->List.rev teams_list
      |_ -> begin   
        let attribute_list = String.split_on_char ',' line 
        in
          (*print_endline line;*) (*ligne d'affichage de la ligne lue*)

          let new_team = { 
            id = int_of_string (List.nth attribute_list 0);
            name = List.nth attribute_list 1;
            wins = int_of_string (List.nth attribute_list 2); 
            losses = int_of_string (List.nth attribute_list 3);
            total_left = int_of_string (List.nth attribute_list 4); 
            left_mi = int_of_string (List.nth attribute_list 5);
            left_csk = int_of_string (List.nth attribute_list 6);
            left_kkr = int_of_string (List.nth attribute_list 7);
            left_dc = int_of_string (List.nth attribute_list 8);
          } in
            readline (new_team::teams_list); 
      end
    in readline []
;;








(*
let cricket_resolution file =
  (*
  - create list of teams
  - create graph file for each team
  - apply ford fulkerson to each graph file
  - conclude if team eliminated or not   
  *)

  let team_list = read_teams file in 
  let rec graph_loop team_list results i =
    match team_list with 
    |[]-> List.rev results
    |a::next -> 
      let graph_file = write_graph ("./graphs/graphEquipe" ^ (string_of_int i)^ ".txt") a team_list in
      (*transformer file en graph*)
      (*recup le numero du node target*)
      let ff_result = algo_ford_fulkerson graph 0 7 in 
      let sum_arcs = 1(*sum of arcs from source*) in 
        if sum_arcs = ff_result then graph_loop next (0::results) (i+1)
        else graph_loop next (1::results) (i+1)
      
  in graph_loop team_list [] 0
;;

*)