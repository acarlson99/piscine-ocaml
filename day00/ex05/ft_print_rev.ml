let ft_print_rev (s : string) : unit =
  let rec f s i =
    if i >= 0 then
      begin
        print_char s.[i];
        f s (i - 1)
      end
  in
  f s (String.length s - 1);
  print_char '\n'

let () =
  ft_print_rev "abcdef";
  ft_print_rev "";
  ft_print_rev "Hello world !";
