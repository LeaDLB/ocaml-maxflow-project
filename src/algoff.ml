open Graph
open Tools

let arc_sature arc = (arc.lbl==0);;

let find_path s t gr = 
  let rec loop s t acu gr =
    match s with 
    |t -> acu
    |x -> 
      let arcs = out_arcs gr x in
      let rec boucle_arc arcs acu =
      match arcs with 
      |[] -> acu
      |arc::reste ->  
        if ( (not (arc.lbl = 0)) && (not (List.mem (arc.tgt) acu)) ) 
          then loop (arc.tgt) t  ((arc.tgt)::acu) gr
          else boucle_arc reste acu
        in boucle_arc arcs acu
  in loop s t [s]
;;

(*
s -> t 
  acu = node list
  regarder le premier arc -> verif les conditions 
                    si ok -> dest::acu + appelle recursion
                    sinon -> remonter et aller voir 2eme arc

  si s=t -> on s'arrete

*)