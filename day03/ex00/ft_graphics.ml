type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square (x : int) (y : int) (size : int) : unit =
  let tlx = (x - (size / 2)) in
  let tly = (y - (size / 2)) in
  Graphics.moveto tlx tly;
  Graphics.lineto (tlx + size) tly;
  Graphics.lineto (tlx + size) (tly + size);
  Graphics.lineto tlx (tly + size);
  Graphics.lineto tlx tly

let draw_tree_node (t : 'a tree) : unit =
  let rec f x y size div t =
    match t with
      Node (a,b,c) -> Graphics.moveto (x-size/2) y; Graphics.draw_string a; draw_square x y size;
                      let d = (600 / div) in
                      f (x - d) (y - size) size (div*2) b;
                      f (x + d) (y - size) size (div*2) c;
    | Nil -> Graphics.moveto (x-size/2) y; Graphics.draw_string "nil"; draw_square x y size
  in
  f 300 384 30 4 t

let () =
  Graphics.open_graph " 600x400";
  draw_tree_node (Node ("abc", Nil, (Node ("def", (Node ("ghi", Nil, Nil)), (Node ("jkl", Nil, Nil))))));
  let rec f () =
    f ()
  in
  f ()
