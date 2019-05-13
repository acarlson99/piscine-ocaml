let gray (n : int) : unit =
  let s =
    [ [0]; [1] ]
  in
  let reverse l q =
    (* TODO: make list reversing function *)
    []
  in
  let rec f n l1 =
    (*
1) Let the list of (n-1)-bit Gray codes be L1. Create another list L2 which is reverse of L1.
2) Modify the list L1 by prefixing a ‘0’ in all codes of L1.
3) Modify the list L2 by prefixing a ‘1’ in all codes of L2.
4) Concatenate L1 and L2. The concatenated list is required list of n-bit Gray codes.
     *)
    let l2 = reverse l1
    in
    l1
  in
  let rec pb l =
    match l with
      x::xs -> print_int x; pb xs
    | [] -> print_char '\n'
  in
  let rec p l =
    match l with
      x::xs -> pb x; p xs
    | [] -> print_string ""
  in
  p (f n s)

let () =
  gray 3
