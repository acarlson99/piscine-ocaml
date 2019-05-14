let uncaesar (s : string) (n : int) : string =
  Cipher.caesar s (-n)

let unrot42 (s : string) : string =
  uncaesar s 42

let rec ft_uncrypt (s : string) (f : (string -> string) list) : string =
  match f with
  | x::xs -> ft_uncrypt (x s) xs
  | _ -> s

let () =
  print_endline (Cipher.caesar "aBcDeF123" (-13));
  print_endline (Cipher.caesar "aBcDeF123" 13);
  print_endline (Cipher.rot42 "abczF!");
  print_endline (unrot42 (Cipher.rot42 "abczF!"));
  print_endline (Cipher.caesar "abczF!" 1);
  print_endline (uncaesar (Cipher.caesar "abczF!" 1) 1);
  print_endline (Cipher.xor "abcdef !" 12);
  print_endline (Cipher.xor (Cipher.xor "abcdef !" 12) 12);
  let cryp = Cipher.ft_crypt "abcdef" [Cipher.rot42; Cipher.rot42; (fun s -> Cipher.xor s 2)] in
  print_endline cryp;
  print_endline (ft_uncrypt cryp [(fun s -> Cipher.xor s 2); unrot42; unrot42])
