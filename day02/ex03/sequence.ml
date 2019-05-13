let sequence (n : int) : string =
  if n < 1 then "Error"
  else let rec count_bois (s : string) (i : int) (n : int) (l : int) : string =
         if i < (l - 1) && s.[i] = s.[i + 1] then count_bois s (i + 1) (n + 1) l
         else if i < l then (string_of_int n ^ String.make 1 s.[i]) ^ (count_bois s (i + 1) 1 l)
         else ""
       in
       let rec f (s : string) (n : int) : string =
         if n = 1 then s
         else f (count_bois s 0 1 (String.length s)) (n - 1)
       in
       f "1" n

let () =
  Printf.printf "%s\n" (sequence 0);
  Printf.printf "%s\n" (sequence 1);
  Printf.printf "%s\n" (sequence 2);
  Printf.printf "%s\n" (sequence 3);
  Printf.printf "%s\n" (sequence 4);
  Printf.printf "%s\n" (sequence 5);
  Printf.printf "%s\n" (sequence 6);
  Printf.printf "%s\n" (sequence 7);
  Printf.printf "%s\n" (sequence 8);
  Printf.printf "%s\n" (sequence 9);
  Printf.printf "%s\n" (sequence 10);
  Printf.printf "%s\n" (sequence 11);
  Printf.printf "%s\n" (sequence 12);
  Printf.printf "%s\n" (sequence 13);
  Printf.printf "%s\n" (sequence 14);
  Printf.printf "%s\n" (sequence 15);
