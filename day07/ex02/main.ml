let () =
  let p = new People.people "Rose" in
  print_endline p#to_string;
  p#talk;
  let pr = new People.people in
  (pr "Jim")#die;
  let doc = new Doctor.doctor "Doctor" 10000 p in
  print_endline @@ doc#to_string;
  doc#testregen;
  doc#use_sonic_screwdriver;
  doc#talk;
  doc#travel_in_time 10 20;
  Random.self_init ();
  let dlk = new Dalek.dalek in
  print_endline dlk#to_string;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#talk;
  dlk#exterminate p;
  print_endline dlk#to_string;
  dlk#exterminate @@ pr "Jim";
  print_endline dlk#to_string;
  dlk#die;
