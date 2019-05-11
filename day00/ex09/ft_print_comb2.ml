let ft_print_comb2 () =
  let rec boi n l =
    if l > 0 || n >= 10 then boi (n / 10) (l - 1);
    print_int (n mod 10);
  in
  let rec f n1 n2 p =
    if n1 < 100 && n2 < 100 then begin
        if p then
          begin
            print_char ',';
            print_char ' '
          end;
        boi n1 1;
        print_char ' ';
        boi n2 1;
        f n1 (n2 + 1) true;
        if n2 == (n1 + 1) then f (n1 + 1) (n1 + 2) true;
      end
  in
  f 0 0 false;
  print_char '\n'

let () =
  ft_print_comb2 ();
