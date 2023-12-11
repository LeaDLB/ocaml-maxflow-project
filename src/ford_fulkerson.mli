open Graph
open Gfile

val list_to_string : int list -> path

val string_to_list : path -> int list

val print_list_arc : 'a arc list -> unit

val arc_sature : int arc -> bool

val find_path : id -> id -> int graph -> path

val get_arc_value : id -> id -> int graph -> int  

val get_min_capacity : path -> int graph -> int

val update_graph : int graph -> path -> int -> int graph

val flow_sum : int graph -> int -> int

val algo_ford_fulkerson : int graph -> int -> int -> (int graph * int)