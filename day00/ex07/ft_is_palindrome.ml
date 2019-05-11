let ft_is_palindrome (s : string) : bool =
  let rec f s l i =
    if l - i < i then true
    else if s.[i] == s.[l - i] then f s l (i + 1)
    else false
  in
  f s (String.length s - 1) 0

let test s =
  if ft_is_palindrome s then print_endline "YEP"
  else print_endline "NOPE"

let () =
  test "abccba";
  test "abcba";
  test "bbcba";
  test "a";
  test "";
  test "radar";
