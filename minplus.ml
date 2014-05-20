open Core.Std

(** Min Plus Semiring *)
module MPS = struct

  exception Negative_value
  
  type t = 
  | Val of int
  | Infinity

  let zero  = Infinity

  let one   = Val 0

  let create i =
    if i >= 0 then Val i
    else raise Negative_value

  let to_string a =
    match a with
    | Val i -> Int.to_string i
    | Infinity -> "Infinity"

  let reduce a = a

  let plus a b =
    match a with
    | Infinity -> b
    | Val a'   ->
      match b with
      | Infinity -> a
      | Val b'   -> Val (min a' b')

  let times a b =
    match a, b with
    | _, Infinity | Infinity, _ -> Infinity
    | Val a', Val b'            -> Val (a' + b')

end

module MMPS = Semiring.Make_Matrix_Semiring(MPS)