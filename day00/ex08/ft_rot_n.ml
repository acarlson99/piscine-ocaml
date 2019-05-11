let ft_rot_n (n : int) (s : string) : string =
  let f n c =
    if c >= 'a' && c <= 'z' then let n = int_of_char c + n in
                                 if n > int_of_char 'z' then char_of_int (n - 26)
                                 else char_of_int n
    else if c >= 'A' && c <= 'Z' then let n = int_of_char c + n in
                                      if n > int_of_char 'Z' then char_of_int (n - 26)
                                      else char_of_int n
    else c
  in
  let p = f (n mod 26)
  in
  String.map p s

let () =
  print_endline (ft_rot_n 39 "ABCZ!");
  print_endline (ft_rot_n 13 "abcz!");
  print_endline (ft_rot_n 1 (ft_rot_n 12 "abcz!"));
  print_endline (ft_rot_n 13 "qPbqr EBG Nhgb")
