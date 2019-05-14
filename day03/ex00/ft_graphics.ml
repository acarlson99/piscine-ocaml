let draw_square (x : int) (y : int) (size : int) : unit =
  let tlx = (x - (size / 2)) in
  let tly = (y - (size / 2)) in
  Graphics.moveto tlx tly;
  Graphics.lineto (tlx + size) tly;
  Graphics.lineto (tlx + size) (tly + size);
  Graphics.lineto tlx (tly + size);
  Graphics.lineto tlx tly

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_tree (t : 'a tree) : unit =
  Graphics.lineto 0 0

let () =
  Graphics.open_graph " 600x400";
  draw_square 100 100 50;
  draw_square 0 0 50;
  draw_square 25 25 50;
  let rec f () =
    f ()
  in
  f ()
