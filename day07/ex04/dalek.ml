class dalek =
object (self)
  val name : string = "Dalek" ^ (String.make 1 @@ char_of_int @@ ((Random.int (112 - 42)) + 42)) ^ (String.make 1 @@ char_of_int @@ ((Random.int (112 - 42)) + 42)) ^ (String.make 1 @@ char_of_int @@ ((Random.int (112 - 42)) + 42))
  val hp : int = 100
  val mutable shield : bool = true

  initializer (print_endline @@ name^" REPORTING FOR DUTY")

  method to_string = "I AM " ^ name ^ " HP " ^ (string_of_int hp) ^ " SHIELD_STATUS: " ^ (string_of_bool shield)
  method talk = print_endline (match Random.int 3 with
                                 0 -> "Explain! Explain!"
                               | 1 -> "Exterminate! Exterminate!"
                               | 2 -> "I obey!"
                               | _ -> "You are the Doctor! You are the enemy of the Daleks!")
  method exterminate (p : People.people) = print_endline "EXTERMINATE"; shield <- (shield <> true); p#die
  method die = print_endline "Emergency Temporal Shift!"
end
