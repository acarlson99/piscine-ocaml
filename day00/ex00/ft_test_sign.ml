let ft_test_sign (n : int) : unit =
  if n < 0 then print_endline "negative"
  else print_endline "positive"

let () =
  ft_test_sign 12;
  ft_test_sign (-12);
  ft_test_sign 42;
