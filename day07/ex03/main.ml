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
  let army = new Army.army [] in
  for i=0 to 5 do
    army#add (new Dalek.dalek) true
  done;
  let talk d = d#talk in
  let announce d = print_endline d#to_string in
  let exterm d = d#exterminate p in
  army#apply talk;
  army#apply announce;
  army#apply exterm;
  army#apply announce;
  print_endline "";
  army#add (new Dalek.dalek) true;
  army#apply announce;
  print_endline "";
  army#add (new Dalek.dalek) false;
  army#apply announce;
  print_endline "";
  army#delete false;
  army#apply announce;
  print_endline "";
  army#delete true;
  army#apply announce;
  for i=0 to 42 do
    army#delete true
  done;
  print_endline "";
  army#apply announce;

  let army = new Army.army [] in
  for i=0 to 5 do
    army#add p true
  done;
  army#apply talk;
  army#apply announce;
  print_endline "";
  army#add (pr "John") true;
  army#apply announce;
  print_endline "";
  army#add (pr "John") false;
  army#apply announce;
  print_endline "";
  army#delete false;
  army#apply announce;
  print_endline "";
  army#delete true;
  army#apply announce;
  for i=0 to 42 do
    army#delete true
  done;
  print_endline "";
  army#apply announce;

  let army = new Army.army [] in
  for i=0 to 5 do
    army#add doc true
  done;
  army#apply talk;
  army#apply announce;
  print_endline "";
  army#add doc true;
  army#apply announce;
  print_endline "";
  army#add doc false;
  army#apply announce;
  print_endline "";
  army#delete false;
  army#apply announce;
  print_endline "";
  army#delete true;
  army#apply announce;
  for i=0 to 42 do
    army#delete true
  done;
  print_endline "";
  army#apply announce;
