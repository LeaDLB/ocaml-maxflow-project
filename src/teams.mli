
(* Type team *)
type team =
  { 
    name : string ;

    (* Wins *)
    wins: int ;

    (* Losses *)
    losses: int ;

    (* Games left *)
    total_left: int ;
    
    (* Games left against MI*)
    left_mi : int;
    
    (* Games left against CSK*)
    left_csk : int;
    
    (* Games left against KKR*)
    left_kkr : int;
    
    (* Games left against DC*)
    left_dc : int;
  }

val write_graph:  string -> team -> team list-> unit
val read_teams : string -> team list
(*val read_text_file : string -> unit*)
val print_team_list : team list -> unit
val other_teams : string -> team list -> team list
val find_match_pairs : team list -> (team*team*int) list