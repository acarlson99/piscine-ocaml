type hour = int
let zero : hour = 0
let rec add (s : hour) (e : hour) : hour = let a = (s + e) mod 24 in
                                           if a < 0 then add a 24
                                           else a
let sub (s : hour) (e : hour) : hour = add (s - e) zero
