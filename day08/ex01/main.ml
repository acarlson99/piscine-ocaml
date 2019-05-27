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

  let tnt = new Molecule.trinitrotoluene in
  print_endline tnt#to_string;

  let water = new Molecule.water in
  print_endline water#to_string;

  let carbon_dioxyde = new Molecule.carbon_dioxyde in
  print_endline carbon_dioxyde#to_string;

  let nitrous_oxide = new Molecule.nitrous_oxide in
  print_endline nitrous_oxide#to_string;

  let glucose = new Molecule.glucose in
  print_endline glucose#to_string;

  let calcium_oxide = new Molecule.calcium_oxide in
  print_endline calcium_oxide#to_string;
