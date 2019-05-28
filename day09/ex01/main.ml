let () =
  Printf.printf "WHEE\n";
  let p = App.zero in
  match p with
    (a,b,c) -> Printf.printf "%s %s %d\n" a b c
