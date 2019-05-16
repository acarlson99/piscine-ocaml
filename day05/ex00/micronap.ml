(* let my_sleep () = Unix.sleep 1 *)

let () =
  if Array.length Sys.argv != 2 then print_endline "Invalid number of arguments"
  else
    Printf.printf "%s\n" (Array.get Sys.argv 1);
  (* for n = 0 to int_of_string (Array.get Sys.argv 1) do
   *   my_sleep ()
   * done *)
