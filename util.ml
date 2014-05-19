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

(** Boolean Semiring *)
module BS = struct

  module B = Core.Blang

  type t = string B.t

  let zero = B.false_

  let one  = B.true_

  let compare_elt = B.compare String.compare

  let compare_product a b =
    let a_list = B.gather_conjuncts a  in
    let b_list = B.gather_conjuncts b  in
    let rec loop a_l b_l = 
      match a_l, b_l with
      | [] , []  -> 0
      | [_], []  | _ :: _, [] -> 1
      | [] , [_] | [], _ :: _ -> -1
      | a_hd :: a_tl, b_hd :: b_tl ->
        let ret = compare_elt a_hd b_hd in
        if  ret = 0 then loop a_tl b_tl
        else ret
    in
    loop a_list b_list

  let rec expand f =
    match f with
    | B.Or  (g, h) -> B.or_ [expand g; expand h]
    | B.And (g, h) -> 
      let g_list = B.gather_disjuncts (expand g) in
      let h_list = B.gather_disjuncts (expand h) in
      let expanded_list = List.fold g_list ~init:[] ~f:(fun l g_component -> 
          List.append l (
            List.map h_list ~f:(fun h_component ->
              B.and_ [g_component; h_component]
            )
          )
        )
      in
      B.or_ expanded_list
    | B.Not f' -> (
      match f' with
      | B.Not f''    -> expand f''
      | B.Or  (g, h) -> expand (B.and_ [B.not_ g; B.not_ h])
      | B.And (g, h) -> expand (B.or_  [B.not_ g; B.not_ h])
      | _            -> expand (B.not_ f'))
    | _ -> f

  let rec reduce_expanded f =
    match f with
    | B.Or (_, _) as sums ->
      let s_list        = List.map (B.gather_disjuncts sums) ~f:reduce_expanded in
      let s_sorted_list = List.sort ~cmp:compare_product s_list in
      B.or_ s_sorted_list
    | B.And (_, _) as products ->
      let p_list = B.gather_conjuncts products in
      B.and_ (List.sort ~cmp:compare_elt p_list)
    | B.Not f' -> (
      match f' with
      | B.Not f'' -> reduce_expanded f''
      | _         -> B.not_ (reduce_expanded f'))
    | _ -> f

  let reduce f = 
    reduce_expanded (expand f)

  let plus a b = 
    if a = b then a
    else reduce (B.or_ [a ; b])

  let times a b =
    if a = b then a
    else reduce (B.and_ [a ; b])

  let create s = 
    reduce (B.t_of_sexp String.t_of_sexp (Sexp.of_string s))

  let to_string f =
    B.sexp_of_t String.sexp_of_t f

end

module MBS = Semiring.Make_Matrix_Semiring(BS)

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