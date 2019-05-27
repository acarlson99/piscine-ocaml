let () =
  let h = new Hydrogen.hydrogen in
  let o = new Oxygen.oxygen in
  let c = new Carbon.carbon in
  let tm = new Thulium.thulium in
  let mo = new Molybdenum.molybdenum in
  print_endline h#to_string;
  print_endline @@ string_of_bool @@ h#equals h;
  print_endline c#to_string;
  print_endline @@ string_of_bool @@ c#equals h;
  print_endline o#to_string;
  print_endline @@ string_of_bool @@ o#equals c;
  print_endline tm#to_string;
  print_endline @@ string_of_bool @@ tm#equals tm;
  print_endline mo#to_string;
  print_endline mo#to_string_verbose;
  print_endline @@ string_of_bool @@ mo#equals c;
