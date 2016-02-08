type prog = (defn list) * expr

and expr =
  | Var of id
  | DVar of id
  | If of expr * expr * expr
  | App of expr * expr list
  | Fun of id list * expr
  | Const of const
and defn = id * expr 
and const =
  | Str of string
  | Int of int
  | False
  | True
and id = string

