class doctor (name : string) (age : int) (sidekick : People.people) =
object (self)
  val name : string = name
  val age : int = age
  val sidekick : People.people = sidekick
  val mutable hp : int = 100
  method to_string = "Hi! I'm " ^ name ^ " age " ^ (string_of_int age) ^ " hp " ^ (string_of_int hp) ^ " traveling with my homie " ^ sidekick#to_string
  method talk = print_endline "Hi! I'm the Doctor!"
  initializer (print_endline "The Doctor is born")
  method travel_in_time (start : int) (arrival : int) = print_endline
"        ___
_______(_@_)_______
| POLICE      BOX |
|_________________|
 | _____ | _____ |
 | |###| | |###| |
 | |###| | |###| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ |$_____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 |       |       |
 *****************"
  (* NOTE: this does not change the Doctor's age because traveling through time does not change the number of days one has existed *)
  method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
  method private regenerate = hp <- 100

  method testregen = hp <- 69;
                     print_endline self#to_string;
                     self#regenerate;
                     print_endline self#to_string;
end
