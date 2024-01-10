(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Values are read as strings. *)
val from_file: path -> string graph


val compute_x: id -> id
val compute_y: id -> id


(* Similarly, we write only a string graph.
 * If necessary, use gmap (to be written by you) to prepare the input graph. *)
val write_file: path -> string graph -> unit


val export: string graph -> path -> unit


(* The format of files is compatible with the files generated by:
   https://algorithms.discrete.ma.tum.de/graph-algorithms/flow-ford-fulkerson/index_en.html
*)
