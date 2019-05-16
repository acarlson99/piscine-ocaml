module Card:
sig
  module Color:
  sig
    type t
    val all : t list
    val toString : t -> string
    val toStringVerbose : t -> string
  end
  module Value:
  sig
    type t
    val all : t list
    val toInt : t -> int
    val toString : t -> string
    val toStringVerbose : t -> string
    val next : t -> t
    val previous : t -> t
  end
end
