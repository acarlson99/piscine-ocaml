module StringSet = Set.Make(String)
class galifrey size =
  let people_ = new Army.army (let rec makeBois n l =
                                 if n = 0 then l
                                 else makeBois (n-1) ((new People.people "Amy")::l)
                               in
                               makeBois size []) in
  object (self)
    val dalek = new Army.army (let rec makeBois n l =
                                 if n = 0 then l
                                 else makeBois (n-1) (new Dalek.dalek::l)
                               in
                               makeBois size [])

    val doctor = new Army.army (let rec makeBois n l partners =
                                  if n = 0 then l
                                  else match partners with
                                         [] -> l
                                       | x::xs -> makeBois (n-1) ((new Doctor.doctor "Doctor" 100 x)::l) xs
                                in
                                makeBois size [] people_#getList)

    val people = people_

    method makeWar =
      let rec f a b =
        match a with
          [] -> ()
        | x::xs -> match b with
                     y::ys -> x#exterminate y; f xs ys
                   | [] -> ()
      in
      people#apply (fun x -> x#talk);
      doctor#apply (fun x -> x#talk);
      doctor#apply (fun x -> print_endline x#to_string);
      f dalek#getList people#getList;
      doctor#apply (fun x -> x#use_sonic_screwdriver);
      dalek#apply (fun x -> x#die)
  end
