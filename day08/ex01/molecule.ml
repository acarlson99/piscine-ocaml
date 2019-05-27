class virtual molecule name (formula_list : Atom.atom list) =
        object(self)
          val name : string = name

          val formula : string =
            let makeBoi s c n =
              s^c^(if n > 1 then string_of_int n else "")
            in
            let rec f l n c s =
              match l with
                x::xs -> if n = 0 then f xs 1 x s
                         else if c = x then f xs (n+1) c s
                         else f xs 1 x (makeBoi s c n)
              | _ -> makeBoi s c n
            in f (List.sort compare @@ List.map (fun a -> a#to_string) formula_list) 0 "" ""

          method to_string = formula
          method equals (a : molecule) : bool = self#to_string = a#to_string
        end

class trinitrotoluene =
object inherit molecule "trinitrotoluene" [new Atom.nitrogen; new Atom.nitrogen; new Atom.nitrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon]
end

class water =
object inherit molecule "water" [new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen]
end

class carbon_dioxyde =
object inherit molecule "carbon dioxyde" [new Atom.carbon; new Atom.oxygen; new Atom.oxygen]
end

class nitrous_oxide =
object inherit molecule "nitrous oxide" [new Atom.nitrogen; new Atom.nitrogen; new Atom.oxygen]
end

class glucose =
object inherit molecule "glucose" [new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen]
end

class calcium_oxide =
object inherit molecule "calcium oxide" [new Atom.calcium; new Atom.oxygen]
end
