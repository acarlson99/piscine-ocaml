type j = { mutable content : string array }

let () =
  if Array.length Sys.argv != 2 then print_endline "usage: ./a.out jokefile"
  else let addToArr (arr : j) (str : string) : unit =
         arr.content <- Array.append arr.content [|str|]
       in
       try let ic = open_in (Sys.argv.(1)) in
           try let a = {content=[| |]} in
               try while true do
                     addToArr a (input_line ic);
                   done
               with End_of_file -> for n=0 to 20 do
                                     print_endline (a.content.(Random.int (Array.length a.content)))
                                   done
           with e -> close_in_noerr ic;
                     print_endline "Error opening file";
       with e -> print_endline "Error opening file";
