(* TODO: finish *)

module type MONAD =
  sig
    type 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end

module Try =
  struct
    type 'a t =
      | Success
      | Failure

    let bind a f =
      match a with
      | Success (x) -> f x
      | Failure (x) -> Failure (x)

    let return x = Success x
  end
