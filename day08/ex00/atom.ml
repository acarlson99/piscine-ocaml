class virtual atom name symbol atomic_number =
        object(self)
          val name : string = name
          val symbol : string = symbol
          val atomic_number : int = atomic_number

          method virtual to_string : string
        end
