type team =
 {  wins: int ;
    losses: int ;
    total_left: int ;

    left_mi = int;
    left_csk = int;
    left_kkr = int;
    left_dc = int;
  }

let mi  = {wins=83; losses=71; total_left=8; left_mi=0; left_csk=1; left_kkr=6; left_dc=1};;
let csk = {wins=80; losses=79; total_left=3; left_mi=1; left_csk=0; left_kkr=0; left_dc=2};;
let kkr = {wins=78; losses=78; total_left=6; left_mi=6; left_csk=0; left_kkr=0; left_dc=0};;
let dc  = {wins=77; losses=82; total_left=3; left_mi=1; left_csk=2; left_kkr=0; left_dc=0};;

let list_teams = [mi,csk,kkr,dc];;

(*Index of teams:
MI  = 1
CSK = 2
KKR = 3
DC  = 4
*)

let create_graph_team team =
  let outfile = open_out "grap"^team^".txt" in 
  Printf.fprintf outfile "hello";
  close_out outfile;; 