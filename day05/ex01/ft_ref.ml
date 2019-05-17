type 'a ft_ref = { mutable contents : 'a }

let return (n : 'a) : 'a ft_ref =
  { contents = n }

let get (r : 'a ft_ref) : 'a =
  r.contents

let set (r : 'a ft_ref) (n : 'a) : unit =
  r.contents <- n

let bind (r : 'a ft_ref) (f : ('a -> 'b ft_ref)) : 'b ft_ref =
  f (get r)

let () =
  let a = return 420 in
  Printf.printf "%d\n" (get a);
  set a 42;
  Printf.printf "%d\n" (get a);
  let b = bind a (fun x -> return "abcdef") in
  Printf.printf "%s\n" (get b);
  set b "ghijkl";
  Printf.printf "%s\n" (get b);
  let c = bind a (fun x -> return (x * 2)) in
  Printf.printf "%d\n" (get c);
  set c 42;
  Printf.printf "%d\n" (get c);
