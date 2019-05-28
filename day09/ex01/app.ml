type project = string * string * int
let zero : project = ("", "", 0)
let combine (a : project) (b : project) : project = zero
let fail (p : project) : project = zero
let succeed (p : project) : project = zero
