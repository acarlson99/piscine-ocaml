let ft_print_alphabet () =
  let rec f n =
    begin
      let c = char_of_int n
      in
      if c <= 'z' then
        begin
          print_char c; f (n + 1)
        end
    end
  in
  begin
    f (int_of_char 'a');
    print_char '\n';
  end

let () =
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
