open Core.Std

(** Boolean Semiring *)
module BS = struct

  module B = Core.Blang

  type t = string B.t

  let zero = B.false_

  let one  = B.true_

  let compare_elt = B.compare String.compare

  let contains s1 s2 =
    let re = Str.regexp_string s2
    in
      try ignore (Str.search_forward re s1 0); true
      with Not_found -> false

  let string_of_product product = 
    let l = B.values product in
    String.of_char_list (List.map l ~f:Char.of_string)

  let compare_product a b =
    let a_str = string_of_product a  in
    let b_str = string_of_product b  in
    let compare_leng = Int.compare (String.length a_str) (String.length b_str) in
      if compare_leng = 0 then String.compare a_str b_str
      else compare_leng

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
      let reduced_strings = ref [] in
      let reduced_l       = ref [] in
      List.iter s_sorted_list ~f:(fun p -> 
        let p_str = string_of_product p in
        if List.exists !reduced_strings ~f:(fun str -> contains p_str str) then ()
        else (
          reduced_strings := (p_str :: !reduced_strings);
          reduced_l       := (p     :: !reduced_l)
        )
      );
      B.or_ (List.rev !reduced_l)
    | B.And (_, _) as products ->
      let p_list        = B.gather_conjuncts products in
      let p_sorted_list = List.sort ~cmp:compare_elt p_list in
      let rec remove_duplicates l acc =
        match l with
        | []  -> List.rev acc
        | [x] -> List.rev (x::acc)
        | x1 :: x2 :: tl ->
          if (compare_elt x1 x2) = 0
          then remove_duplicates (x2 :: tl) acc
          else remove_duplicates (x2 :: tl) (x1 :: acc)
      in
      B.and_ (remove_duplicates p_sorted_list [])
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
    Sexp.to_string (B.sexp_of_t String.sexp_of_t f)

  let test s =
    to_string (create s)

end

module MBS = Semiring.Make_Matrix_Semiring(BS)