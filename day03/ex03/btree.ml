type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_bst (t : 'a tree) : bool =
  let rec f (t : 'a tree) (mx : 'a option) (mn : 'a option) =
    match t with
      Node (a,b,c) ->
       if mx != None && Some a > mx then false
       else if mn != None && Some a < mn then false
       else f b (Some a) mn && f c mx (Some a)
    | Nil -> true
  in
  f t None None

let is_perfect (t : 'a tree) : bool =
  let rec f t d lvl =
    match t with
      Node (a,b,c) -> if b = Nil && c = Nil then (d = (lvl + 1))
                      else if b = Nil || c = Nil then false
                      else (f b d (lvl + 1) && f c d (lvl + 1))
    | Nil -> true
  in
  let rec getdepth t d =
    match t with
      Node (a,b,_) -> getdepth b (d + 1)
    | Nil -> d
  in
  f t (getdepth t 0) 0

let is_balanced (t : 'a tree) : bool =
  let rec getheight t =
    match t with
      Nil -> (-1)
    | Node (a,b,c) -> let ah = getheight c and bh = getheight b in
                      if ah > bh then ah + 1
                      else bh + 1
  in
  let rec f t =
    match t with
      Nil -> true
    | Node (a,b,c) -> let lh = getheight b and rh = getheight c in
                      abs (lh - rh) <= 1 && f b && f c
  in
  f t

let rec search_bst (v : 'a) (t : 'a tree) : bool =
  match t with
    Node (c,l,r) -> if c = v then true
                    else if v < c then search_bst v l
                    else search_bst v r
  | Nil -> false

let rec add_bst (v : 'a) (t : 'a tree) : 'a tree =
  match t with
    Node (c,l,r) -> if v < c then Node (c, add_bst v l, r)
                    else if v > c then Node (c, l, add_bst v r)
                    else t
  | Nil -> Node (v,Nil,Nil)

let delete_bst (v : 'a) (t : 'a tree) : 'a tree =
  Nil

let () =
  let chk t =
    Printf.printf "\n%b\n" (is_bst t);
    Printf.printf "%b\n" (is_perfect t);
    if is_balanced t then Printf.printf "Perfectly balanced as all things should be\n"
    else Printf.printf "Unbalanced REEEEEEEEEEEE\n";
    Printf.printf "69 in tree? %b\n" (search_bst 69 t);
    let nt = add_bst 69 t in
    Printf.printf "69 in tree? %b\n" (search_bst 69 nt)
  in
  let t = Node (10, Node (7, Nil, Nil), Node (11, Nil, Node (12, Node (50, Nil, Nil), Nil))) in (* false *)
  chk t;
  Printf.printf "%b\n" (search_bst 10 t);
  Printf.printf "%b\n" (search_bst 18 t);
  let t = Node (10, Node (7, Nil, Node (9, Nil, Nil)), Node (11, Nil, Node (12, Nil, Nil))) in (* true *)
  chk t;
  Printf.printf "%b\n" (search_bst 10 t);
  Printf.printf "%b\n" (search_bst 18 t);
  let t = Node (10, Node (1, Nil, Nil), Node (20, Nil, Nil)) in (* perfect *)
  chk t;
  Printf.printf "%b\n" (search_bst 10 t);
  Printf.printf "%b\n" (search_bst 18 t);
