type j = { mutable content : (float array * string) list }

(* TODO: finish *)

let examples_of_file (filepath : string) : (float array * string) list =
  let a = {content=[]} in
  let addToList (str : string) : unit =
    let l = String.split_on_char ',' str in
    a.content <- a.content @ [(Array.of_list (List.mapi (fun i x -> if (i+1) = List.length l then 0.0 else float_of_string x) l), "g")]
                               (* lst.content <- lst.content @ [(Array.of_list (List.map float_of_string (String.split_on_char ',' str)), "g")] *)
  in
  try let ic = open_in (filepath) in
      try while true do
            addToList (input_line ic);
          done;
          a.content
      with End_of_file -> a.content
  with e -> print_endline "Unable to open file";
            Printf.eprintf "ERROR %s %s\n" (Printexc.to_string e) (Printexc.get_backtrace ());
            []

let () =
  if Array.length Sys.argv != 2 then Printf.printf "Enter a file\n"
  else let l = examples_of_file Sys.argv.(1) in
       List.iter (fun x -> Array.iter (Printf.printf "%f ") (fst x); Printf.printf "%s\n" (snd x)) l
