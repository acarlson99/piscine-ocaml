let rec caesar (s : string) (n : int) : string =
  let ro n c =
    match c with
      'A' .. 'Z' -> char_of_int ((int_of_char c - (int_of_char 'A') + n) mod 26 + (int_of_char 'A'))
    | 'a' .. 'z' -> char_of_int ((int_of_char c - (int_of_char 'a') + n) mod 26 + (int_of_char 'a'))
    | _ -> c
  in
  let f n =
    if n = 0 then s
    else String.map (ro n) s
  in
  if n < 0 then caesar s (n + 26)
  else f (n mod 26)

let rot42 (s : string) : string =
  caesar s 42

let xor (s : string) (n : int) : string =
  String.map (fun c -> char_of_int ((int_of_char c) lxor n)) s

let rec ft_crypt (s : string) (f : (string -> string) list) : string =
  match f with
  | x::xs -> ft_crypt (x s) xs
  | _ -> s
