(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)

let clone_nodes gr = n_fold gr new_node empty_graph;;

let gmap gr f = e_fold gr (fun acc arc -> new_arc acc {src = arc.src; tgt = arc.tgt; lbl = f (arc.lbl)}) (clone_nodes gr);;

let add_arc gr src dst v = 
  let arc = find_arc gr src dst in 
  match arc with 
  |None -> new_arc gr {src = src; tgt = dst; lbl = v}
  |Some x -> new_arc gr {src = x.src; tgt = x.tgt; lbl = x.lbl + v};;


(* Replace _gr and _f by gr and f when you start writing the real function. *)