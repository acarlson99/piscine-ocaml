(* ocamlbuild -lib unix micronap.native *)

let my_sleep () = Unix.sleep 1

let () =
  if Array.length Sys.argv != 2 then exit 69
  else
    for n = 1 to (try int_of_string (Array.get Sys.argv 1) with e -> exit 69) do
      my_sleep ()
    done
