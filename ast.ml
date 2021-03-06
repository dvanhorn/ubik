type prog = (defn list) * expr

and expr =
  | Var of id
  | If of expr * expr * expr
  | App of expr * expr list
  | Fun of id list * expr
  | Const of const
  | Match of expr * (pat * expr) list 
and pat = id * (id list)
and defn = 
  | Def of id * expr 
  | Struct of id * id list
and const =
  | Str of string
  | Int of int
  | False
  | True
and id = string

