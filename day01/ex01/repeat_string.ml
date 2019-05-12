let rec repeat_string ?(str : string option) (n : int) : string =
  match str with
  | Some s -> if n > 0 then s ^ repeat_string ~str:s (n - 1)
              else if n < 0 then "Error"
              else ""
  | None -> repeat_string ~str:"x" n

let () =
  print_endline (repeat_string 12);
  print_endline (repeat_string ~str:"A" 1);
  print_endline (repeat_string ~str:"A" (-12));
