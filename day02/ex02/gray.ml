let gray (n : int) : unit =
  if n < 1 then print_endline "Error"
  else let s =
         [ [0]; [1] ]
       in
       let rec reverse l acc =
         match l with
           x::xs -> reverse xs (x::acc)
         | _ -> acc
       in
       let rec pad l n =
         match l with
           x :: xs -> (n :: x) :: (pad xs n)
         | _ -> []
       in
       let rec concat l1 l2 =
         match l1 with
           x::xs -> x :: (concat xs l2)
         | _ -> l2
       in
       let rec f n l1 =
         if n == 1 then l1
         else f (n - 1) (concat (pad l1 0) (pad (reverse l1 []) 1))
       in
       let rec pb l =
         match l with
           x::xs -> print_int x; pb xs
         | [] -> print_string ""
       in
       let rec p l r =
         match l with
           x::xs -> if r then print_char ' '; pb x; p xs true
           | [] -> print_string "\n"
       in
       p (f n s) false

let () =
  gray 1;
  print_char '\n';
  gray 2;
  print_char '\n';
  gray 3;
