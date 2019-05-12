let rec tak (x : int) (y : int) (z : int) : int =
  if y < x then tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
  else z

let () =
  Printf.printf "%d\n" (tak 1 2 3);
  Printf.printf "%d\n" (tak 5 23 7);
  Printf.printf "%d\n" (tak 9 1 0);
  Printf.printf "%d\n" (tak 1 1 1);
  Printf.printf "%d\n" (tak 0 42 0);
  Printf.printf "%d\n" (tak 23498 98734 98776);
  (* Printf.printf "%d\n" (tak 40 10 1); commented out because long *)
