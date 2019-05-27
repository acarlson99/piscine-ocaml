class ['a] army lads =
object (self)
  val mutable bois : 'a list = lads
  method add (boi : 'a) (front : bool) : unit =
    if front then bois <- boi::bois
    else bois <- bois@[boi]
  method delete (front : bool) : unit =
    let rec d b c = 
      match b with
        x::[] -> c
      | x::xs -> d xs (c @ [x])
      | _ -> c
    in
    if front then bois <- (match bois with
                             x::xs -> xs
                           | [] -> [])
    else bois <- d bois []

  method apply f = List.iter f bois
  method getList = bois
end
