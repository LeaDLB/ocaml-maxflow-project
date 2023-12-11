open Graph
open Tools

let list_to_string l = 
  let string_list = List.map string_of_int l in String.concat "," string_list ;;
  
let string_to_list l = 
  let string_list = String.split_on_char ',' l in 
  List.map int_of_string string_list;;

let print_list_arc list = 
 let _list = List.map (fun arc -> let n1 = arc.src in let n2 = arc.tgt in Printf.printf "%d->%d\n%!" n1 n2) list in
 ();;

let arc_sature arc = (arc.lbl=0);;
 

let find_path s t gr = 
  let rec loop s t path nodes_parcourus gr =
    if (s=t) then (t::path) 
    else
      begin
      let arcs = out_arcs gr s in
      let rec boucle_arc arcs path nodes_parcourus =
        
        let nodes_parcourus = s::nodes_parcourus in
        match arcs with 
        |[] -> []
        |arc::reste when ( (not (arc_sature arc)) && (not (List.mem (arc.tgt) nodes_parcourus)) ) ->
          let res = (loop (arc.tgt) t (s::path) nodes_parcourus gr) in
          if res = [] then boucle_arc reste path nodes_parcourus else res
        |_::reste -> boucle_arc reste path nodes_parcourus 
       in  boucle_arc arcs path nodes_parcourus 
      end
  in let int_list = loop s t [] [] gr in
  list_to_string (List.rev int_list)
;;

let get_arc_value n1 n2 gr = 
  let arc = find_arc gr n1 n2 in
  match arc with
  |None -> 0  
  |Some x -> x.lbl;;


  (* Parcours du path pour récupérer la capacité minimale. Path = string. *)
let get_min_capacity p gr =
  let path = string_to_list p in
  let rec loop path gr acu =
    match path with
    |[]-> acu
    |n1::rest -> 
      match rest with
      |[]->acu
      |n2::_ -> let value = min (get_arc_value n1 n2 gr) acu in 
                loop (rest) gr value
  in loop path gr max_int;;


let update_graph gr p value =
  let path = (string_to_list p) in 
  let rec loop gr path value =
    match path with 
    |[] -> empty_graph
    |n1::rest -> (
      match rest with
      |[]-> gr
      |n2::_-> 
        let new_graph = (add_arc gr n1 n2 (-value)) in 
        loop new_graph rest value)
  in loop gr path value 
;;


let flow_sum gr src =
  let arcs = out_arcs gr src in
  let rec loop arcs acu = 
  match arcs with
  |[]-> acu
  |a::rest -> loop rest (acu+a.lbl)
  in loop arcs 0
;;


let algo_ford_fulkerson gr src tgt =
  let flow_init = flow_sum gr src in
  let rec loop gr =
    let path = (find_path src tgt gr) in 
    match path with
    |""-> gr
    |_ -> let capa =  (get_min_capacity path gr) in 
          let new_graph = (update_graph gr path capa) in 
          loop new_graph 
  in let result_graph = loop gr in
  let flow = flow_sum result_graph src in
  (result_graph,(flow_init-flow)) ;;