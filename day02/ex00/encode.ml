let encode (l : 'a list) : (int * 'a) list =
  [ (3, 4) ]

(* val encode : 'a list -> (int * 'a) list =
 *   0 *)

let () =
  List.iter (Printf.printf "%d ") [1; 2; 3; 4];
