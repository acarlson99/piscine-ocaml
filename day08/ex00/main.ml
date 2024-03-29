let () =
  let h = new Atom.hydrogen in
  let o = new Atom.oxygen in
  let c = new Atom.carbon in
  let tm = new Atom.thulium in
  let mo = new Atom.molybdenum in
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
