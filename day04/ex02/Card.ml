module Color =
  struct

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

  end

module Value =
  struct

    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    let all : t list =
      [T2;T3;T4;T5;T6;T7;T8;T9;T10;Jack;Queen;King;As]

    let toInt (a : t) : int =
      match a with
        T2 -> 1
      | T3 -> 2
      | T4 -> 3
      | T5 -> 4
      | T6 -> 5
      | T7 -> 6
      | T8 -> 7
      | T9 -> 8
      | T10 -> 9
      | Jack -> 10
      | Queen -> 11
      | King -> 12
      | As -> 13

    let toString (a : t) : string =
      match a with
        T2 -> "2"
      | T3 -> "3"
      | T4 -> "4"
      | T5 -> "5"
      | T6 -> "6"
      | T7 -> "7"
      | T8 -> "8"
      | T9 -> "9"
      | T10 -> "10"
      | Jack -> "J"
      | Queen -> "Q"
      | King -> "K"
      | As -> "A"

    let toStringVerbose (a : t) : string =
      match a with
        T2 -> "2"
      | T3 -> "3"
      | T4 -> "4"
      | T5 -> "5"
      | T6 -> "6"
      | T7 -> "7"
      | T8 -> "8"
      | T9 -> "9"
      | T10 -> "10"
      | Jack -> "Jack"
      | Queen -> "Queen"
      | King -> "King"
      | As -> "As"

    let next (a : t) : t =
      let rec f n l =
        match l with
          x::xs -> if n = 1 then x
                   else f (n - 1) xs
        | _ -> invalid_arg "No next value for As"
      in
      f (toInt a + 1) all

    let previous (a : t) : t =
      let rec f n l =
        match l with
          x::xs -> if n = 1 then x
                   else f (n - 1) xs
        | _ -> invalid_arg "No next value for As"
      in
      if a = T2 then invalid_arg "No previous value for T2"
      else f (toInt a - 1) all

  end

type t = (Value.t * Color.t)

let newCard (v : Value.t) (c : Color.t) : t =
  (v,c)

let all = List.rev (List.fold_left (fun a cl -> List.fold_left (fun ab vl -> (vl,cl) :: ab) a Value.all) [] Color.all)
