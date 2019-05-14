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




type aminoacid =
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

type protein = aminoacid list

let rec generate_bases_triplets (r : rna) : (nucleobase * nucleobase * nucleobase) list =
  match r with
    a::b::c::xs -> (a,b,c) :: generate_bases_triplets xs
  | _ -> []

let string_of_protein (p : protein) : string =
  let rec f p sp st =
    if sp && p != [] then f p false (st ^ " ")
    else match p with
         | Stop::xs -> f xs true (st ^ "EOT")
         | Ala::xs -> f xs true (st ^ "Alanine")
         | Arg::xs -> f xs true (st ^ "Arginine")
         | Asn::xs -> f xs true (st ^ "Asparagine")
         | Asp::xs -> f xs true (st ^ "Aspartique")
         | Cys::xs -> f xs true (st ^ "Cysteine")
         | Gln::xs -> f xs true (st ^ "Glutamine")
         | Glu::xs -> f xs true (st ^ "Glutamique")
         | Gly::xs -> f xs true (st ^ "Glycine")
         | His::xs -> f xs true (st ^ "Histidine")
         | Ile::xs -> f xs true (st ^ "Isoleucine")
         | Leu::xs -> f xs true (st ^ "Leucine")
         | Lys::xs -> f xs true (st ^ "Lysine")
         | Met::xs -> f xs true (st ^ "Methionine")
         | Phe::xs -> f xs true (st ^ "Phenylalanine")
         | Pro::xs -> f xs true (st ^ "Proline")
         | Ser::xs -> f xs true (st ^ "Serine")
         | Thr::xs -> f xs true (st ^ "Threonine")
         | Trp::xs -> f xs true (st ^ "Tryptophane")
         | Tyr::xs -> f xs true (st ^ "Tyrosine")
         | Val::xs -> f xs true (st ^ "Valine")
         | _ -> st
  in
  f p false ""



let decode_arn (r : rna) : protein =
  let rec f r l =
    match r with
    | (U,A,A)::(U,A,G)::(U,G,A)::xs -> l @ [Stop]
    | (G,C,A)::(G,C,C)::(G,C,G)::(G,C,U)::xs -> f xs (l @ [Ala])
    | (A,G,A)::(A,G,G)::(C,G,A)::(C,G,C)::(C,G,G)::(C,G,U)::xs -> f xs (l @ [Arg])
    | (A,A,C)::(A,A,U)::xs -> f xs (l @ [Asn])
    | (G,A,C)::(G,A,U)::xs -> f xs (l @ [Asp])
    | (U,G,C)::(U,G,U)::xs -> f xs (l @ [Cys])
    | (C,A,A)::(C,A,G)::xs -> f xs (l @ [Gln])
    | (G,A,A)::(G,A,G)::xs -> f xs (l @ [Glu])
    | (G,G,A)::(G,G,C)::(G,G,G)::(G,G,U)::xs -> f xs (l @ [Gly])
    | (C,A,C)::(C,A,U)::xs -> f xs (l @ [His])
    | (A,U,A)::(A,U,C)::(A,U,U)::xs -> f xs (l @ [Ile])
    | (C,U,A)::(C,U,C)::(C,U,G)::(C,U,U)::(U,U,A)::(U,U,G)::xs -> f xs (l @ [Leu])
    | (A,A,A)::(A,A,G)::xs -> f xs (l @ [Lys])
    | (A,U,G)::xs -> f xs (l @ [Met])
    | (U,U,C)::(U,U,U)::xs -> f xs (l @ [Phe])
    | (C,C,C)::(C,C,A)::(C,C,G)::(C,C,U)::xs -> f xs (l @ [Pro])
    | (U,C,A)::(U,C,C)::(U,C,G)::(U,C,U)::xs -> f xs (l @ [Ser])
    | (A,C,A)::(A,C,C)::(A,C,G)::(A,C,U)::xs -> f xs (l @ [Thr])
    | (U,G,G)::xs -> f xs (l @ [Trp])
    | (U,A,C)::(U,A,U)::xs -> f xs (l @ [Tyr])
    | (G,U,A)::(G,U,C)::(G,U,G)::(G,U,U)::xs -> f xs (l @ [Val])
    | _::xs -> f xs l
    | _ -> l
  in
  f (generate_bases_triplets r) []

let () =
  let hx = generate_helix 500 in
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
  List.iter printBoi (generate_bases_triplets rn);
  Printf.printf "\n";
  Printf.printf "%s\n" (string_of_protein [Ala; Asn; Stop]);
  let prtn = decode_arn rn in
  Printf.printf "%s\n" (string_of_protein prtn);
