type phosphate = string
type deoxyribose = string
type nucleobase = 
  | A
  | T
  | C
  | G
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

let () =
  let nt = generate_nucleotide 'A' in
  Printf.printf "%s %s " nt.p nt.d;
  match nt.n with
    A -> Printf.printf "A\n"
  | T -> Printf.printf "T\n"
  | C -> Printf.printf "C\n"
  | G -> Printf.printf "G\n"
  | None -> Printf.printf "None\n"
