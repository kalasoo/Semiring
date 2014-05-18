(** Semiring *)
module type S = sig
  
  type t
  (** The type of a semiring. *)

  val zero   : t

  val one    : t

  val reduce : t -> t

  val plus   : t -> t -> t

  val times  : t -> t -> t

end

(** Functor building an implementation of the Martrix Semiring
   given a Semiring. *)
module Make_Matrix_Semiring (Semiring : S) = struct

  type t = Semiring.t array array

  exception Different_size
  exception Invalid_size

  let zero n = Array.make_matrix n n Semiring.zero

  let one  n = 
    let matrix = zero n in
    for i = 0 to n - 1 do
      matrix.(i).(i) <- Semiring.one
    done;
    matrix

  let size a =
    let row_num = Array.length a in
    if row_num = 0 then 0
    else let col_num = Array.length a.(0) in
      if row_num = col_num then row_num
      else raise Invalid_size

  let same_size a b =
    let a_size = size a in
    let b_size = size b in
    if a_size = b_size then Some a_size
    else None

  let plus a b =
    match same_size a b with
    | None -> raise Different_size
    | Some n ->
      let matrix = zero n in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          matrix.(i).(j) <- Semiring.plus a.(i).(j) b.(i).(j)
        done;
      done;
      matrix

  let inner_times n a_row b_col =
    let result = ref (Semiring.zero) in
    Array.iteri (fun i a_i -> 
      result := Semiring.plus !result (Semiring.times a_i b_col.(i)) 
    ) a_row;
    !result

  let column n j matrix =
    let col = Array.make n Semiring.zero in
    for i = 0 to n - 1 do
      col.(i) <- matrix.(i).(j)
    done;
    col

  let times a b =
    match same_size a b with
    | None -> raise Different_size
    | Some n ->
      let matrix = zero n in
      for i = 0 to n - 1 do
        let a_row = a.(i) in
        for j = 0 to n - 1 do
          let b_col = column n j b in
          matrix.(i).(j) <- inner_times n a_row b_col
        done;
      done;
      matrix

end
