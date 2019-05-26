let () =
  let p = new People.people "Rose" in
  print_endline p#to_string;
  let pr = new People.people in
  (pr "Jim")#die
