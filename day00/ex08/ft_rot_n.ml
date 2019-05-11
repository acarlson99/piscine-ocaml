let ft_rot_n (n : int) (s : string) : string =
  let f n c =
    if c >= 'a' && c <= 'z' then char_of_int ((int_of_char c + n) mod (int_of_char 'a' - int_of_char 'z') + int_of_char 'a')
    else if c >= 'A' && c <= 'Z' then c
    else c
  in
  let p = f n
  in
  String.map p s

let () =
  print_endline (ft_rot_n 3 "abc!")
