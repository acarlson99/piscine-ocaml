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
  print_endline @@ string_of_bool @@ calcium_oxide#equals calcium_oxide;
  print_endline @@ string_of_bool @@ calcium_oxide#equals glucose;

  let methane = new Alkane.methane in
  print_endline methane#to_string;
  print_endline methane#getName;
  let ethane = new Alkane.ethane in
  print_endline ethane#to_string;
  print_endline ethane#getName;
  let propane = new Alkane.propane in
  print_endline propane#to_string;
  print_endline propane#getName;
  let butane = new Alkane.butane in
  print_endline butane#to_string;
  print_endline butane#getName;
  let pentane = new Alkane.pentane in
  print_endline pentane#to_string;
  print_endline pentane#getName;
  let hexane = new Alkane.hexane in
  print_endline hexane#to_string;
  print_endline hexane#getName;
  let septane = new Alkane.septane in
  print_endline septane#to_string;
  print_endline septane#getName;
  let octane = new Alkane.octane in
  print_endline octane#to_string;
  print_endline octane#getName;
  let nonane = new Alkane.nonane in
  print_endline nonane#to_string;
  print_endline nonane#getName;
  let decane = new Alkane.decane in
  print_endline decane#to_string;
  print_endline decane#getName;
  let undecane = new Alkane.undecane in
  print_endline undecane#to_string;
  print_endline undecane#getName;
  let dodecane = new Alkane.dodecane in
  print_endline dodecane#to_string;
  print_endline dodecane#getName;
  let thing = new Alkane.alkane 209 in
  print_endline thing#to_string;
  print_endline thing#getName;
