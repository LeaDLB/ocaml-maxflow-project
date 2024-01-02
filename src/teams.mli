(* Type team *)
type team =
  { (* Wins *)
    wins: int ;

    (* Losses *)
    losses: int ;

    (* Games left *)
    total_left: int ;
    
    (* Games left against MI*)
    left_mi = int;
    
    (* Games left against CSK*)
    left_csk = int;
    
    (* Games left against KKR*)
    left_kkr = int;
    
    (* Games left against DC*)
    left_dc = int;
  }