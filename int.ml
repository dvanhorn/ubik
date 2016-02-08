open Big_int

type t = big_int

let succ = succ_big_int
let pred = pred_big_int
let square = square_big_int
let mult = mult_big_int
let add = add_big_int
let eq_int = eq_big_int
let pow = power_big_int_positive_big_int
let int_of = big_int_of_int
let string_of_int = string_of_big_int
(* small int pow *)
(* 
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
     let b = pow a (n / 2) in
     b * b * (if n mod 2 = 0 then 1 else a)
 *)
