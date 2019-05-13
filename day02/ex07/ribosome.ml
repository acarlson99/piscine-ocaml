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




type protein =
  | Stop (* UAA, UAG, UGA               : End of translation *)
  | Ala (* GCA, GCC, GCG, GCU           : Alanine *)
  | Arg (* AGA, AGG, CGA, CGC, CGG, CGU : Arginine *)
  | Asn (* AAC, AAU                     : Asparagine *)
  | Asp (* GAC, GAU                     : Aspartique *)
  | Cys (* UGC, UGU                     : Cysteine *)
  | Gln (* CAA, CAG                     : Glutamine *)
  | Glu (* GAA, GAG                     : Glutamique *)
  | Gly (* GGA, GGC, GGG, GGU           : Glycine *)
  | His (* CAC, CAU                     : Histidine *)
  | Ile (* AUA, AUC, AUU                : Isoleucine *)
  | Leu (* CUA, CUC, CUG, CUU, UUA, UUG : Leucine *)
  | Lys (* AAA, AAG                     : Lysine *)
  | Met (* AUG                          : Methionine *)
  | Phe (* UUC, UUU                     : Phenylalanine *)
  | Pro (* CCC, CCA, CCG, CCU           : Proline *)
  | Ser (* UCA, UCC, UCG, UCU           : Serine *)
  | Thr (* ACA, ACC, ACG, ACU           : Threonine *)
  | Trp (* UGG                          : Tryptophane *)
  | Tyr (* UAC, UAU                     : Tyrosine *)
  | Val (* GUA, GUC, GUG, GUU           : Valine *)

let rec generate_bases_triplets (r : rna) : (nucleobase * nucleobase * nucleobase) list =
  match r with
    a::b::c::xs -> (a,b,c) :: generate_bases_triplets xs
  | _ -> []

let string_of_protein (p : protein) : string =
  ""

let () =
  let hx = generate_helix 17 in
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
  let rn = generate_rna hx in
  List.iter printBoi rn;
  Printf.printf "\n";
  let printBoi (a,b,c) =
    Printf.printf "(%s-%s-%s) " (getBoi a) (getBoi b) (getBoi c)
  in
  List.iter printBoi (generate_bases_triplets rn)
