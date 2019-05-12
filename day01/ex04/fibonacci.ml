let rec fibonacci (n : int) : int =
  let rec f la lb n =
    match n with
      0 -> la
    | 1 -> lb
    | _ -> f lb (la + lb) (n - 1)
  in
  if n < 0 then -1
  else f 0 1 n

let () =
  Printf.printf "%d\n" (fibonacci 0);
  Printf.printf "%d\n" (fibonacci 1);
  Printf.printf "%d\n" (fibonacci 2);
  Printf.printf "%d\n" (fibonacci 3);
  Printf.printf "%d\n" (fibonacci 4);
  Printf.printf "%d\n" (fibonacci 5);
  Printf.printf "%d\n" (fibonacci 6);
  Printf.printf "%d\n" (fibonacci 7);
  Printf.printf "%d\n" (fibonacci (-2));
