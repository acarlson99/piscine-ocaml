type 'a mutarr = { mutable contents : 'a }

let eu_dist (a : float array) (b : float array) : float =
  let create_arr =
    let r = { contents = [| |] } in
    for i=0 to (max (Array.length a) (Array.length b)) - 1 do
      r.contents <- Array.append r.contents [|(a.(i), b.(i))|];
    done;
    r
  in
  sqrt (Array.fold_left (fun l r -> ((fst r -. snd r) ** 2.) +. l) 0. create_arr.contents)

let () =
  Printf.printf "%f\n" (eu_dist [|2.;(-1.)|] [|(-2.);2.|]);
  Printf.printf "%f\n" (eu_dist [|1.;1.|] [|2.;1.;|])
