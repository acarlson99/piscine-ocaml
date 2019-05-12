(* TODO: what does "We don't have to handle duplicates in lists" mean? *)
let rec crossover (a : 'a list) (b : 'a list) : 'a list =
  if a == [] || b == [] then []
  else let rec check (a : 'a) (b : 'a list) : bool =
         match b with
           [x] when x == a -> true
         |  x::xs when x == a -> true
         | x::xs -> check a xs
         | _ -> false
       in
       let rec f a b =
         match a with
           [x] when check x b -> [x]
         | x::xs when check x b -> (x :: (f xs b))
         | x::xs -> f xs b
         | _ -> []
       in
       f a b

let () =
  Printf.printf "[ ";
  List.iter (Printf.printf "%d ") (crossover [1;2;3] [1;3;2]);
  Printf.printf "]\n";
  Printf.printf "[ ";
  List.iter (Printf.printf "%c ") (crossover ['a'; 'b'; 'c'] ['b'; 'a']);
  Printf.printf "]\n";
