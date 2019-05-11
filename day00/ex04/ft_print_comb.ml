let ft_print_comb () =
  let rec boi n l =
    if l > 0 || n >= 10 then boi (n / 10) (l - 1);
    print_int (n mod 10);   (* TODO: check if this is an operator or function *)
  in
  let rec f prev start len p =
    if (len + start) > 9 then print_string ""
    else if len <= 0 then
      begin
        if p == 1 then print_string ", ";
        boi ((prev * 10) + start) 2;
        f prev (start + 1) len 1
      end
    else
      begin
        f ((prev * 10) + start) (start + 1) (len - 1) p;
        f prev (start + 1) len 1
      end
  in
  f 0 0 2 0;
  print_string "\n"

let () =
  ft_print_comb ();
