let rec ft_sum (f : int -> float) (lo : int) (hi : int) : float =
  if hi < lo then nan
  else if hi == lo then f lo
  else f lo +. ft_sum f (lo + 1) hi

let () =
  Printf.printf "%f\n" (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  Printf.printf "%f\n" (ft_sum (fun i -> ((float_of_int i) /. 2.0)) 1 10);
  Printf.printf "%f\n" (ft_sum (fun i -> (float_of_int i)) 1 10);
