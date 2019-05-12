let rec ackermann (m : int) (n : int) : int =
  match m, n with
    0, _ -> n + 1
  | _, 0 when m > 0 -> ackermann (m - 1) 1
  | _, _ when m > 0 && n > 0 -> ackermann (m - 1) (ackermann m (n - 1))
  | _, _ -> 0

let () =
  Printf.printf "%d\n" (ackermann 1 1);
  Printf.printf "%d\n" (ackermann 1 2);
  Printf.printf "%d\n" (ackermann 3 2);
  Printf.printf "%d\n" (ackermann 3 4);
  Printf.printf "%d\n" (ackermann 4 1);
