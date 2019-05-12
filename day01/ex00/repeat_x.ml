let rec repeat_x (n : int) : string =
  if n < 0 then "Error"
  else if n == 0 then ""
  else "x" ^ repeat_x (n - 1)

let () =
  print_endline (repeat_x 42);
  print_endline (repeat_x (-42));
  print_endline (repeat_x 0);
