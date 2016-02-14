type t = Z.t

let succ = Z.succ
let pred = Z.pred
let square x = Z.pow x 2
let mult = Z.mul
let add = Z.add
let eq_int = Z.equal
let pow x y = Z.pow x (Z.to_int y)
let int_of = Z.of_int
let string_of_int = Z.to_string

(* small int pow *)
(* 
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
     let b = pow a (n / 2) in
     b * b * (if n mod 2 = 0 then 1 else a)
 *)
