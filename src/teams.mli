open Gfile


(* Type team *)
type team =
  { 
    id: int;
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

val print_team_list : team list -> unit
val other_teams : string -> team list -> team list
val find_match_pairs : team list -> (team*team*int) list
val write_nodes: out_channel -> int -> int -> unit
val write_source_arcs: out_channel -> (team*team*int) list -> int -> int -> unit
val write_end_arcs: team -> int -> int -> out_channel -> team list -> int -> unit
val write_graph:  string -> team -> team list-> unit
val read_teams : string -> team list



val cricket_resolution : string -> int list