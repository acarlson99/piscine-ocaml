class alkane (n : int) =
object(self)
  val name : string = (match n with
                         1  -> "meth"
                       | 2  -> "eth"
                       | 3  -> "prop"
                       | 4  -> "but"
                       | 5  -> "pent"
                       | 6  -> "hex"
                       | 7  -> "sept"
                       | 8  -> "oct"
                       | 9  -> "non"
                       | 10 -> "dec"
                       | 11 -> "undec"
                       | 12 -> "dodec"
                       | _  -> "NOOOOOOOOOOOOOOOOOOOPELOL") ^ "ane"
  val formula : string = "C"^(if n = 1 then "" else string_of_int n)^"H"^(string_of_int @@ 2 * n + 2)

  method to_string : string = formula
  method getName : string = name
  method equals (a : alkane) : bool = self#to_string = a#to_string
end

class methane =
object inherit alkane 1
end

class ethane =
object inherit alkane 2
end

class propane =
object inherit alkane 3
end

class butane =
object inherit alkane 4
end

class pentane =
object inherit alkane 5
end

class hexane =
object inherit alkane 6
end

class septane =
object inherit alkane 7
end

class octane =
object inherit alkane 8
end

class nonane =
object inherit alkane 9
end

class decane =
object inherit alkane 10
end

class undecane =
object inherit alkane 11
end

class dodecane =
object inherit alkane 12
end
