let () =
  let t_next a =
    try Printf.printf "%s\n" (Value.toStringVerbose (Value.next a));
    with Invalid_argument "No next value for As" -> Printf.printf "No next value for As\n"
  in
  let t_prev a =
    try Printf.printf "%s\n" (Value.toStringVerbose (Value.previous a));
    with Invalid_argument "No previous value for T2" -> Printf.printf "No previous value for T2\n"
  in
  
  t_prev Value.T2;
  t_prev Value.As;
  t_next Value.T2;
  t_next Value.As;
  Printf.printf "%s\n" (Value.toString Value.As);
