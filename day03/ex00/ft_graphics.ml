let draw_square (x : int) (y : int) (size : int) : unit =
  let tlx = (x - (size / 2)) in
  let tly = (y - (size / 2)) in
  Graphics.moveto tlx tly;
  Graphics.lineto (tlx + size) tly;
  Graphics.lineto (tlx + size) (tly + size);
  Graphics.lineto tlx (tly + size);
  Graphics.lineto tlx tly

let () =
  Graphics.open_graph " 600x400";
  draw_square 100 100 50;
  while true do
    Printf.printf ""
  done
