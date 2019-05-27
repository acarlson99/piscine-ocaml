class virtual atom name symbol atomic_number =
        object(self)
          val name : string = name
          val symbol : string = symbol
          val atomic_number : int = atomic_number

          method to_string : string = symbol
          method to_string_verbose : string = name^" "^(string_of_int atomic_number)
          method equals (a : atom) : bool = self#to_string = a#to_string
        end
