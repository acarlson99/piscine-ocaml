type t = Spade | Heart | Diamond | Club

let all : t list =
  [Spade; Heart; Diamond; Club]

let toString (i : t) : string =
  match i with
    Spade -> "S"
  | Heart -> "H"
  | Diamond -> "D"
  | Club-> "C"

let toStringVerbose (i : t) : string =
  match i with
    Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"
