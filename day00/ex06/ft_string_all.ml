let ft_string_all (f : char -> bool) (s : string) : bool =
  let rec m f s i =
    if i >= 0 then
      begin
        if (f s.[i]) then m f s (i - 1)
        else false
      end
    else true
  in
  m f s (String.length s - 1)

let is_digit n =
  n >= '0' && n <= '9'

let test f s =
  if ft_string_all f s then print_endline "YEP"
  else print_endline "NOPE"

let () =
  test is_digit "abcdef";
  test is_digit "012345";
  test is_digit "012a345"
