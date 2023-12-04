open Graph

let list_to_string l = 
    let string_list = List.map string_of_int l in String.concat "," string_list ;;
  


let arc_sature arc = (arc.lbl=0);;
 

let find_path s t gr = 
  let rec loop s t acu gr =
    if (s=t) then acu else
      begin
      let arcs = out_arcs gr s in
      let rec boucle_arc arcs acu =
        match arcs with 
        |[] -> acu
        |arc::reste ->  
          if ( (not (arc_sature arc)) && (not (List.mem (arc.tgt) acu)) ) 
            then loop (arc.tgt) t  ((arc.tgt)::acu) gr
            else boucle_arc reste acu
        in boucle_arc arcs acu
      end
  in let int_list = loop s t [s] gr in
  list_to_string (List.rev int_list)
;;


let get_arc_value n1 n2 gr = 
  let arc = find_arc gr n1 n2 in
  match arc with
  |None -> 0  
  |Some x -> x.lbl;;


  (* Parcours du path pour récupérer la capacité minimale. Path = string. 
let get_min_capacity path gr =
  let rec loop path gr index acu =
    match String.length path with
    |(index+1)-> acu
    |_ -> begin let n1 = int_of_char (path.[index]) in
                let n2 = int_of_char (path.[index+1]) in
                let value = min (get_arc_value n1 n2 gr) acu in 
                loop path gr (index+1) value
    end
  in loop path gr 0 max_int*)


(*
s -> t 
  acu = node list
  regarder le premier arc -> verif les conditions 
                    si ok -> dest::acu + appelle recursion
                    sinon -> remonter et aller voir 2eme arc

  si s=t -> on s'arrete

*)