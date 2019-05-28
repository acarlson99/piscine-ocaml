module type MONOID =
  sig
    type element
    val zero1 : element
    val zero2 : element
    val mul : element -> element -> element
    val add : element -> element -> element
    val div : element -> element -> element
    val sub : element -> element -> element
    val power : element -> int -> element
    val fact : element -> element
  end
  
module Calc =
  functor (M : MONOID) ->
  struct
    let add = M.add
    let sub = M.sub
    let mul = M.mul
    let div = M.div
    let power = M.power
    let fact = M.fact
  end
  
module INT =
  struct
    type element = int

    let zero1 = 0
    let zero2 = 1
    let add a b = a + b
    let sub a b = a - b
    let mul a b = a * b
    let div a b = a / b
    let power a b = int_of_float @@ (float_of_int a) ** (float_of_int b)
    let fact a = let rec f a n =
                   if a = 1 then n
                   else f (a - 1) (n * a)
                 in
                 f a 1
  end

module FLOAT =
  struct
    type element = float

    let zero1 = 0.
    let zero2 = 1.
    let add a b = a +. b
    let sub a b = a -. b
    let mul a b = a *. b
    let div a b = a /. b
    let power a b = a ** (float_of_int b)
    let fact a = let rec f a n =
                   if a <= 1. then n
                   else f (a -. 1.) (n *. a)
                 in
                 f a 1.
  end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () =
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))
