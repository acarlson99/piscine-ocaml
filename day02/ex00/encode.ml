let encode (l : 'a list) : (int * 'a) list =
  let rec f (l : 'a list) (n : int) : (int * 'a) list =
    match l with
      [] -> []
    | a :: b :: c when a == b -> f (b::c) (n + 1)
    | a :: b :: c -> ((n, a) :: (f (b::c) 1))
    | a :: b -> (n, a) :: (f b 1)
  in
  f l 1

let () =
  let print_tup ((x,y) : (int * int)) : unit =
    Printf.printf "(%i %i) " x y
  in
  Printf.printf "[ ";
  List.iter print_tup (encode [1; 1; 2; 3; 4]);
  Printf.printf "]\n";
  let print_tup ((x,y) : (int * char)) : unit =
    Printf.printf "(%i %c) " x y
  in
  Printf.printf "[ ";
  List.iter print_tup (encode ['a'; 'a'; 'b'; 'b'; 'b']);
  Printf.printf "]\n";
  Printf.printf "[ ";
  List.iter print_tup (encode []);
  Printf.printf "]\n";
