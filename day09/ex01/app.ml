type project = string * string * int
let zero : project = ("", "", 0)
let combine (a : project) (b : project) : project =
  match a, b with
    (a,b,c), (d,e,f) -> (a^d,(if (c+f)/2 >= 80 then "succeed" else "fail"),(c+f)/2)
let fail (p : project) : project = match p with (a,b,c) -> (a,"fail",0)
let succeed (p : project) : project = match p with (a,b,c) -> (a,"succeed",80)
let print_proj (p : project) : unit = match p with (a,b,c) -> print_endline @@ a^" "^b^" "^(string_of_int c)
