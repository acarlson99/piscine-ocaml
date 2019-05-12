let rec ft_sum (f : int -> float) (lo : int) (hi : int) : float =
  let rec fn f lo hi total =
    if hi < lo then nan
    else if hi == lo then f lo +. total
    else fn f (lo + 1) hi (total +. (f lo))
  in
  fn f lo hi 0.

let () =
  Printf.printf "%f\n" (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  Printf.printf "%f\n" (ft_sum (fun i -> ((float_of_int i) /. 2.0)) 1 10);
  Printf.printf "%f\n" (ft_sum (fun i -> (float_of_int i)) 1 10);
