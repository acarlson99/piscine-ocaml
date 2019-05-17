let () =
  let jokes_arr = [|"Python2"; "My life"; "lol I'm having a jolly time"; "*chuckles* Well actually"; "Kill me"; "Hehe nice"|] in
  Random.self_init ();
  print_endline (jokes_arr.(Random.int (Array.length jokes_arr)));
