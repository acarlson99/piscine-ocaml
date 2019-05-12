(* TODO: check this *)
let leibniz_pi (d : float) : int =
  if d <= 0.0 then (-1)
  else let a n =
         if n < 0.0 then (-.n) else n
       in
       if d < 0.0 then -1
       else let rec f (i : int) (t : float) (lst : float) : int =
              if a ((4.0 *. (atan 1.0)) -. (4.0 *. t)) <= d then i
              else f (i + 1) ((((-1.0) ** float_of_int i) /. (float_of_int ((2 * i) + 1))) +. t) t
            in
            f 0 0.0 0.0

let () =
  Printf.printf "%d\n" (leibniz_pi 0.00000001);
  Printf.printf "%d\n" (leibniz_pi 0.0000001);
  Printf.printf "%d\n" (leibniz_pi 0.000001);
  Printf.printf "%d\n" (leibniz_pi 0.00001);
  Printf.printf "%d\n" (leibniz_pi 0.0001);
  Printf.printf "%d\n" (leibniz_pi 0.001);
  Printf.printf "%d\n" (leibniz_pi 0.01);
  Printf.printf "%d\n" (leibniz_pi 0.1);
  Printf.printf "%d\n" (leibniz_pi 1.0);
  Printf.printf "%d\n" (leibniz_pi (-1.0));
