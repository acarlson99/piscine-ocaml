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
  type t
  val newCard : Value.t -> Color.t -> t
  val getValue : t -> Value.t
  val getColor : t -> Color.t
  val all : t list
  val allSpades : t list
  val allHearts : t list
  val allDiamonds : t list
  val allClubs : t list
  val toString : t -> string
  val toStringVerbose : t -> string
  val compare : t -> t -> int
  val max : t -> t -> t
  val min : t -> t -> t
  val best : t list -> t
  val isOf : t -> Color.t -> bool
  val isSpade : t -> bool
  val isHeart : t -> bool
  val isDiamond : t -> bool
  val isClub : t -> bool
end
type t
val newDeck : unit -> t
val toStringList : t -> string list
val toStringListVerbose : t -> string list
val drawCard : t -> (Card.t * t)
