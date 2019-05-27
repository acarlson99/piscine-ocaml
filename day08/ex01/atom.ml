class virtual atom name symbol atomic_number =
        object(self)
          val name : string = name
          val symbol : string = symbol
          val atomic_number : int = atomic_number

          method to_string : string = symbol
          method to_string_verbose : string = name^" "^(string_of_int atomic_number)
          method equals (a : atom) : bool = self#to_string = a#to_string
        end

class carbon =
object inherit atom "carbon" "C" 6
end

class aluminum =
object inherit atom "aluminum" "Al" 13
end

class calcium =
object inherit atom "calcium" "Ca" 20
end

class hydrogen =
object inherit atom "hydrogen" "H" 1
end

class molybdenum =
object inherit atom "molybdenum" "Mo" 42
end

class oxygen =
object inherit atom "oxygen" "O" 8
end

class thulium =
object inherit atom "thulium" "Tm" 69
end

class nitrogen =
object inherit atom "nitrogen" "N" 7
end
