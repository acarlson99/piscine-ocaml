let rec hfs_f (n : int) : int =
  if n < 0 then -1
  else match n with
         0 -> 1
       | _ -> n - hfs_m (hfs_f (n - 1))
            
and hfs_m (n : int) : int =
  if n < 0 then -1
  else match n with
         0 -> 0
       | _ -> n - hfs_f (hfs_m (n - 1))

let () =
  Printf.printf "%d\n" (hfs_m 0);
  Printf.printf "%d\n" (hfs_f 0);
  Printf.printf "%d\n" (hfs_m 4);
  Printf.printf "%d\n" (hfs_f 4);
  Printf.printf "%d\n" (hfs_m 11);
  Printf.printf "%d\n" (hfs_f 11);
  Printf.printf "%d\n" (hfs_f 19);
