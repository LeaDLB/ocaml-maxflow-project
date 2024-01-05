

(* Type team *)
type team =
  { (* Name *)
    name: string ;
    
    (* Wins *)
    wins: int ;

    (* Losses *)
    losses: int ;
    
    (* Games left *)
    total_left: int ;
    
    (* Games left against MI*)
    left_mi: int;
    
    (* Games left against CSK*)
    left_csk: int;
    
    (* Games left against KKR*)
    left_kkr: int;
    
    (* Games left against DC*)
    left_dc: int;
  } ;;


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
              if (b.name="csk") then loop2 ((a,b,a.left_csk)::pairs) next else
              if (b.name="mi")  then loop2 ((a,b,a.left_mi )::pairs) next else 
              if (b.name="kkr") then loop2 ((a,b,a.left_kkr)::pairs) next else 
                loop2 ((a,b,a.left_dc )::pairs) next 
        in let pairs = loop2 pairs next in
        loop pairs next
  in loop [] list
;;
          


let test = other_teams "csk" team_list;;

let pairs = find_match_pairs test;;









