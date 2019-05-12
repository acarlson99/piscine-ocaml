let rec converges (f : 'a -> 'a) (x : 'a) (n : int) : bool =
  if n < 0 then false
  else if x == (f x) then true
  else if n == 0 then false
  else converges f (f x) (n - 1)

let () =
  Printf.printf "%b\n" (converges (( * ) 2) 2 5);
  Printf.printf "%b\n" (converges (fun x -> x / 2) 2 5);
  Printf.printf "%b\n" (converges (fun x -> x / 2) 2 2);
  Printf.printf "%b\n" (converges (fun x -> x / 2) 2 1);
