let rec ft_countdown (n : int) : unit =
  if n > 0
  then
    begin
      print_int n;
      print_char '\n';
      ft_countdown (n - 1)
    end
  else
    begin
      print_int 0;
      print_char '\n';
    end


let () =
  ft_countdown 3;
  print_endline "";
  ft_countdown 0;
  print_endline "";
  ft_countdown (-1);
