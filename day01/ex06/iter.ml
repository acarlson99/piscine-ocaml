let rec iter (f : int -> int) (x : int) (n : int) : int =
  if n < 0 then -1
  else match n with
         0 -> x
       | _ -> f (iter f x (n - 1))

let () =
  Printf.printf "%d\n" (iter (fun x -> x * x) 2 4);
  Printf.printf "%d\n" (iter (fun x -> x * 2) 2 4);
  Printf.printf "%d\n" (iter (fun x -> x * 2) 2 0);
  Printf.printf "%d\n" (iter (fun x -> x * 2) 2 1);
  Printf.printf "%d\n" (iter (fun x -> x * 2) 2 (-1));
