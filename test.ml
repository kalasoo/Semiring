open Util

let a = BS.create "a";;
let b = BS.create "b";;
let c = BS.create "c";;

let a_and_b = BS.create "(and a b)";;
let b_and_a = BS.create "(and b a)";;

let a_or_b = BS.create "(or a b)";;
let b_or_a = BS.create "(or b a)";;

let ab = BS.create "(and (or a b) (or b a))"

let c_or_d = BS.create "(or c d)";;

let a_and_c = BS.create "(and a c)";;