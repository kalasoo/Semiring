open Core.Std

(** Min Plus Semiring *)
module MPS = struct

  exception Negative_value
  
  type t = 
  | Val of int
  | Infinity

  let create i =
    if i >= 0 then Val i
    else raise Negative_value

  let zero  = Infinity

  let one   = Val 0  

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

module Make_Boolean (Elt : 'a)

(** Boolean Semiring *)
module BS = struct

  include Core.Blang

  let zero = constant false

  let one  = constant true

  let compare_elt = compare String.compare

  let compare_product a b =
    let a_list = gather_conjuncts a  in
    let b_list = gather_conjuncts b  in
    let rec loop a_l b_l = 
      match a_l, b_l with
      | [], [] -> 0
      | a', [] -> 1
      | [], b' -> -1
      | a_hd :: a_tl, b_hd :: b_tl ->
        let ret = compare_elt a_hd b_hd in
        if  ret = 0 then loop a_tl b_tl
        else ret
    in
    loop a_list b_list

  let rec expand f =
    match f with
    | Or  (g, h) -> or_ [expand g; expand h]
    | And (g, h) -> 
      let g_list = gather_disjuncts (expand g) in
      let h_list = gather_disjuncts (expand h) in
      let expanded_list = List.fold g_list ~init:[] ~f:(fun l g_component -> 
          List.append l (
            List.map h_list ~f:(fun h_component ->
              and_ [g_component; h_component]
            )
          )
        )
      in
      or_ expanded_list
    | Not f' -> (
      match f' with
      | Not f''    -> expand f''
      | Or  (g, h) -> expand (and_ [not_ g; not_ h])
      | And (g, h) -> expand (or_  [not_ g; not_ h])
      | _          -> expand (not_ f'))
    | _ -> f

  let rec reduce_expanded f =
    match f with
    | Or (g, h) as sums ->
      let s_list        = List.map (gather_disjuncts sums) ~f:reduce_expanded in
      let s_sorted_list = List.sort compare_product s_list in
      or_ s_sorted_list
    | And (g, h) as products ->
      let p_list = gather_conjuncts products in
      and_ (List.sort compare_elt p_list)
    | Not f' -> (
      match f' with
      | Not f'' -> reduce_expanded f''
      | _       -> not_ (reduce_expanded f'))
    | _ -> f


  let rec reduce f = 
    reduce_expanded (expand f)

  let create s = 
    reduce (t_of_sexp String.t_of_sexp (Sexp.of_string s))

  let to_string f =
    sexp_of_t String.sexp_of_t f

  let plus a b = 
    if a = b then a
    else reduce (or_ [a ; b])

  let times a b =
    if a = b then a
    else reduce (and_ [a ; b])

end