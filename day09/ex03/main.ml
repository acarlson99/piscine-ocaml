module type MONAD =
  sig
    type t = Success of 'a
           | Failure of exn
    val return : 'a -> MONAD
    val return : 'a Success -> 'a
  end
