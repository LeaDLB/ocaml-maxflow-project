open Graph
open Gfile

val list_to_string : id list -> path

val arc_sature : int arc -> bool

val find_path : id -> id -> int graph -> path

val get_arc_value : id -> id -> int graph -> int  

(*val get_min_capacity : path -> 'a graph -> 'a*)