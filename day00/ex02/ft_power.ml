let rec ft_power (n : int) (pow : int) : int =
  if pow <= 0 then 1
  else n * (ft_power n (pow - 1))

let () =
  print_int (ft_power 8 2);
  print_char '\n';
