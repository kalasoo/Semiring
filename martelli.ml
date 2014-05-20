open Core.Std

(** Martelli Semiring *)
module MS = struct
  
  module S = Set.Make(String)

  include Set.Make(S)

  let zero = add empty S.empty

  let one = empty

  let reduce a = 
    let not_subset set = for_all a ~f:(fun a_set -> not (S.subset a_set set) || S.equal a_set set) in
    filter a ~f:not_subset

  let plus a b =
    let fold_element s a_set = fold b ~init:s ~f:(fun s_iter b_set -> add s_iter (S.union a_set b_set)) in
    reduce (fold a ~init:empty ~f:fold_element )

  let times a b =
    reduce (union a b)

  let create s =
    reduce (t_of_sexp (Sexp.of_string s))

  let to_string a =
    Sexp.to_string (sexp_of_t a)

end

module MMS = Semiring.Make_Matrix_Semiring(MS)