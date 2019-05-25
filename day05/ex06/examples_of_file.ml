type j = { mutable content : (float array * string) list }

(* TODO: finish *)

let examples_of_file (filepath : string) : (float array * string) list =
  let addToList (lst : j) (str : string) : unit =
    lst.content <- lst.content @ [(Array.of_list (List.map float_of_string (String.split_on_char ',' str)), "g")]
  in
  try let ic = open_in (filepath) in
      let a = {content=[]} in
      try while true do
            addToList a (input_line ic);
          done
      with End_of_file -> print_endline "EOF";
                          j.content
  with e -> print_endline "Unable to open file"

let () =
  List.iter (fun x -> Array.iter (Printf.printf "%f ") (fst x); Printf.printf "%s\n" (snd x)) (examples_of_file "./ionosphere.data")
