type phosphate = string
type deoxyribose = string
type nucleobase = 
  | A
  | T
  | C
  | G
  | U
  | None

type nucleotide = {p : phosphate;
                   d : deoxyribose;
                   n : nucleobase}

let generate_nucleotide (c : char) : nucleotide =
  match c with
    'A' -> {p = "phosphate"; d = "deoxyribose"; n = A}
  | 'T' -> {p = "phosphate"; d = "deoxyribose"; n = T}
  | 'C' -> {p = "phosphate"; d = "deoxyribose"; n = C}
  | 'G' -> {p = "phosphate"; d = "deoxyribose"; n = G}
  | _ -> {p = "phosphate"; d = "deoxyribose"; n = None}

type helix = nucleotide list

let generate_helix (n : int) : helix =
  let getChar n =
    match n with
      0 -> 'A'
    | 1 -> 'T'
    | 2 -> 'C'
    | 3 -> 'G'
    | _ -> 'F'
  in
  if n < 1 then []
  else let rec f n l =
         if n == 0 then l
         else f (n - 1) (generate_nucleotide (getChar (Random.int 4)) :: l)
       in
       f n []

let rec helix_to_string (hx : helix) : string =
  let getBase b =
    match b with
      A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | _ -> "None"
  in
  let rec f hx st sp =
    match hx with
      x::xs when sp -> f xs (st ^ "-" ^ getBase x.n) true
    | x::xs -> f xs (st ^ getBase x.n) true
    | _ -> st
  in
  f hx "" false

let complementary_helix (hx : helix) : helix =
  let getComp n =
    match n with
      A -> 'T'
    | T -> 'A'
    | C -> 'G'
    | G -> 'C'
    | _ -> 'F'
  in
  let rec f hx =
    match hx with
      x::xs -> generate_nucleotide (getComp x.n) :: f xs
    | _ -> []
  in
  f hx

type rna = nucleobase list

let generate_rna (hx : helix) : rna =
  let getBoi n =
    match n with
      A -> U
    | T -> A
    | C -> G
    | G -> C
    | _ -> None
  in
  let rec f hx =
    match hx with
      x::xs -> (getBoi x.n) :: f xs
    | _ -> []
  in
  f hx

let () =
  let hx = generate_helix 15 in
  let getBoi n =
    match n with
      A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | _ -> "None"
  in
  let printBoi n =
    Printf.printf "%s " (getBoi n.n)
  in
  List.iter printBoi hx;
  Printf.printf "\n";
  let printBoi n =
    Printf.printf "%s " (getBoi n)
  in
  List.iter printBoi (generate_rna hx);
  Printf.printf "\n";
