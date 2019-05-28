let () =
  Printf.printf "WHEE\n";
  let p = App.zero in
  App.print_proj p;
  let pn = ("Paper", "succeed", 100) in
  App.print_proj pn;
  let pother = App.combine p pn in
  App.print_proj pother;
  App.print_proj @@ App.succeed pother;
  App.print_proj @@ App.fail pother;
