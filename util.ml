type positive_int = 
| Val of int
| Infinity

(** Min Plus Semiring *)
module MPS : 
  (Semiring.S with type t := positive_int)
= struct
  
  let zero  = Infinity

  let one   = Val 0  

  let reduce a = a

  let plus a b =
    match a with
    | Infinity -> b
    | Val a'   ->
      match b with
      | Infinity -> a
      | Val b'   -> Val (max a' b')

  let times a b =
    match a, b with
    | _, Infinity | Infinity, _ -> Infinity
    | Val a', Val b'            -> Val (a' + b')

end

(** Boolean Semiring *)
module BS :
  (Semiring.S with type t := bool)
= struct

  let zero = false

  let one  = true

  let reduce a = a

  let plus = (||)

  let times = (&&)
  
end