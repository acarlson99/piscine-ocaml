type j = { mutable content : string array }

let () =
  if Array.length Sys.argv != 2 then print_endline "usage: ./a.out jokefile"
  else let addToArr (arr : j) (str : string) : unit =
         arr.content <- Array.append arr.content [|str|]
       in
       let a = {content=[| |]} in


       let ic = open_in (Sys.argv.(1)) in
       try
         let line = input_line ic in
         print_endline line;
         addToArr a line;
         print_endline (a.content.(Random.int (Array.length a.content)));
       with e ->
         close_in_noerr ic;
         print_endline "Error opening file";
