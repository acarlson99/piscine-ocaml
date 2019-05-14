type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_tree (t : 'a tree) : unit =
  let draw_square (x : int) (y : int) (size : int) : unit =
    let tlx = (x - (size / 2)) in
    let tly = (y - (size / 2)) in
    Graphics.moveto tlx tly;
    Graphics.lineto (tlx + size) tly;
    Graphics.lineto (tlx + size) (tly + size);
    Graphics.lineto tlx (tly + size);
    Graphics.lineto tlx tly
  in
  let rec f x y size div t =
    match t with
      Node (a,b,c) -> Graphics.moveto (x-size/2) y; Graphics.draw_string a; draw_square x y size;
                      let d = (600 / div) in
                      Graphics.moveto (x - d) (y - size + size/2);
                      Graphics.lineto x (y - size/2);
                      f (x - d) (y - size) size (div*2) b;
                      Graphics.moveto (x + d) (y - size + size/2);
                      Graphics.lineto x (y - size/2);
                      f (x + d) (y - size) size (div*2) c;
    | Nil -> Graphics.moveto (x-size/2) y; Graphics.draw_string "nil"; draw_square x y size
  in
  f 300 384 30 4 t

let rec size (t : 'a tree) : int =
  match t with
    Node (a,b,c) -> size b + size c + 1
  | Nil -> 0

let rec height (t : 'a tree) : int =
  match t with
    Nil -> (-1)
  | Node (a,b,c) -> let ah = height c and bh = height b in
                    if ah > bh then ah + 1
                    else bh + 1

let () =
  let t = (Node ("abc", Nil, (Node ("def", (Node ("ghi", Nil, Nil)), (Node ("jkl", Nil, Nil)))))) in
  print_endline (string_of_int (size t));
  print_endline (string_of_int (height t))
