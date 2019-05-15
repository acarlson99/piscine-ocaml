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
  true

let is_balanced (t : 'a tree) : bool =
  true

let search_bst (v : 'a) (t : 'a tree) : bool =
  true

let add_bst (v : 'a) (t : 'a tree) : 'a tree =
  Nil

let delete_bst (v : 'a) (t : 'a tree) : 'a tree =
  Nil

let () =
  let t = Node (10, Node (7, Nil, Node (42, Nil, Nil)), Node (11, Nil, Node (12, Nil, Nil))) in (* false *)
  Printf.printf "%b\n" (is_bst t);
  let t = Node (10, Node (7, Nil, Node (9, Nil, Nil)), Node (11, Nil, Node (12, Nil, Nil))) in (* true *)
  Printf.printf "%b\n" (is_bst t);
