let () =
  let p = new People.people "Rose" in
  print_endline p#to_string;
  let pr = new People.people in
  (pr "Jim")#die;
  let doc = new Doctor.doctor "Doctor" 10000 p in
  print_endline @@ doc#to_string;
  doc#testregen;
