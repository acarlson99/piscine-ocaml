class people name =
object (self)
  (* val name : string = n *)
  val hp : int = 100
  initializer (print_endline ("Oh the pain of being alive " ^ self#to_string))
  method to_string = name ^ " " ^ (string_of_int hp)
  method talk = print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")
  method die = print_endline "Aaaarghh!"
end
