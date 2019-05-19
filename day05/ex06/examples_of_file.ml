let examples_of_file (filepath : string) : (float array * string) list =
  []

let () =
  List.iter (fun x -> Array.iter (Printf.printf "%f ") (fst x); Printf.printf "%s\n" (snd x)) (examples_of_file "./ionosphere.data")
