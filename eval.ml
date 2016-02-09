
open Ast
open Int

type value = 
  | False
  | True
  | Str of string 
  | Int of Int.t
  | Sub1
  | Add1
  | Sqr
  | Plus
  | Times
  | Equal
  | Expt
  | Cons
  | IsCons
  | Car
  | Cdr
  | Null
  | IsNull
  | List
  | Pair of value * value
  | Proc of id list * expr * env 
  | Rec of (value option) ref 
  | StructPred of id
  | StructCons of id * int
  | StructAcc  of id * int
  | StructVal  of id * value array
  (* Only in compiled code *)
  | CProc of int * (venv -> value) * venv

and env = (id * value) list
and venv = value list
and ntag = Var | Def
and nenv = (ntag * id) list

let ext (xs : id list) (vs : value list) (r : env) : env =
  List.combine xs vs @ r

let i2i (op : Int.t -> Int.t -> Int.t) : (value list -> value) =
  function [Int i; Int j] -> Int (op i j)

let i1i (op : Int.t -> Int.t) : (value list -> value) =
  function [Int i] -> Int (op i)

let i2b (op : Int.t -> Int.t -> bool) : (value list -> value) =
  function [Int i; Int j] -> 
	   match (op i j) with
	    | true -> True
	    | false -> False
	

let add1 = i1i succ
let sub1 = i1i pred
let is_eq = i2b eq_int
		
let make_struct_env (x : id) (xs : id list) : env =
  (x ^ "?", StructPred x) ::
    (x, StructCons (x, List.length xs)) ::
      (List.mapi (fun i f -> (x ^ "-" ^ f, StructAcc (x, i))) xs)

let make_struct_nenv (x : id) (xs : id list) : nenv =
  List.map (fun (x,v) -> (Var, x))  (make_struct_env x xs)

let make_struct_venv (x : id) (xs : id list) : venv =
  List.map (fun (x,v) -> v)  (make_struct_env x xs)

let rec eval_p (p : prog) (r : env) : value =
  match p with
  | [], e -> eval_e e r
  | d::ds, e -> eval_p (ds, e) (eval_d d r)
and eval_d (d : defn) (r : env) : env =
  match d with
  | Def (x, e) -> 
     let b = ref None in
     let r'= (x, Rec b) :: r in
     let v = eval_e e r' in
     b := Some v;
     r'
  | Struct (x, xs) ->
     make_struct_env x xs @ r
and eval_e (e : expr) (r : env) : value =
  match e with
  | Const (Str s) -> Str s
  | Const (Int i) -> Int (int_of i)
  | Const False -> False
  | Const True -> True
  | Var x -> (match (List.assoc x r) with
	      | Rec x -> (let Some v = !x in v)
	      | v -> v)
  | If (e1, e2, e3) ->
     (match eval_e e1 r with
      | False -> eval_e e3 r
      | _ -> eval_e e2 r)
  | Match (e, cs) ->
     let StructVal (x, vs) = eval_e e r in
     let rec m cs =
       let ((y, ys), e) :: cs = cs in
       if x=y 
       then eval_e e ((List.combine ys (Array.to_list vs)) @ r)
       else m cs
     in
     m cs
  | Fun (xs, e) -> Proc (xs, e, r)
  | App (e, es) ->
     let v  = eval_e e r in
     let vs = eval_es es r in
     apply v vs
and eval_es (es : expr list) (r : env) : value list =
  List.map (fun e -> eval_e e r) es
and apply (f : value) (vs : value list) : value =
  match f with
  | Proc (xs, e, r) -> eval_e e (ext xs vs r)
  | IsCons ->
     (match vs with 
      | [Pair _] -> True
      | [x] -> False)
  | IsNull ->
     (match vs with 
      | [Null] -> True
      | [x] -> False)
  | Cons  -> let [x; y] = vs in Pair (x, y)
  | Car   -> let [Pair (x, y)] = vs in x
  | Cdr   -> let [Pair (x, y)] = vs in y
  | Sub1  -> sub1 vs
  | Add1  -> add1 vs
  | Sqr   -> i1i square vs
  | Times -> i2i mult vs
  | Plus  -> i2i add vs
  | Expt  -> i2i pow vs
  | Equal -> is_eq vs
  | List  -> (List.fold_right (fun v vs -> Pair (v, vs)) vs Null)
  | StructPred x -> 
     (match vs with
      | [StructVal (y, vs)] -> if x = y then True else False
      | _ -> False)
  | StructCons (x, i) -> StructVal (x, Array.of_list vs)
  | StructAcc (x, i) ->
     let [StructVal (y, vs)] = vs in vs.(i)

let r0 : env = 
  [("=",     Equal);
   ("*",     Times);
   ("+",     Plus);
   ("sub1",  Sub1);
   ("add1",  Add1);
   ("expt",  Expt);
   ("sqr",   Sqr);
   ("cons",  Cons);
   ("car",   Car);
   ("cdr",   Cdr);
   ("null",  Null);
   ("list",  List);
   ("null?", IsNull);
   ("pair?", IsCons)]

let n0 = List.map (fun (x,v) -> (Var, x)) r0
let v0 = List.map (fun (x,v) -> v) r0


let rec find (x : id) (rn : nenv) : (ntag * int) =
  let rec find_acc (rn : nenv) (i : int) : (ntag * int) =
    match rn with
    | [] -> failwith ("Unbound variable " ^ x)
    | (t, y) :: rn -> if x = y then (t, i) else find_acc rn (i+1)
  in
  find_acc rn 0

let rec compile_p (p : prog) (rn : nenv) : venv -> value =
  match p with
  | ds, e -> 
     let (rn, cds) =       
       List.fold_left (fun (rn, cds) d -> 
		       let (rn',cd) = compile_d d rn in
		       (rn', fun rv -> cd (cds rv)))
		      (rn, (fun (rv : venv) -> rv))
		      ds
     in
     let c = compile_e e rn in
     fun r -> c (cds r)
and compile_d (d : defn) (rn : nenv) : (nenv * (venv -> venv)) =
  match d with
  | Def (x, e) -> 
     let c = compile_e e ((Def, x) :: rn) in
     (((Def, x) :: rn),
      fun r ->
      let b = ref None in
      let r'= (Rec b) :: r in
      b := Some (c r');
      r')
  | Struct (x, xs) ->
     make_struct_nenv x xs @ rn,
     (@) (make_struct_venv x xs)
     
and compile_e (e : expr) (rn : nenv) : venv -> value =
  match e with
  | Const (Str s) -> let sv = Str s in fun r -> sv
  | Const (Int i) -> 
     let j = Int (int_of i) in
     fun r -> j
  | Const False -> let f = False in fun r -> f
  | Const True -> let t = True in fun r -> t
  | Var x -> 
     (match find x rn with
      | Var, i -> fun r -> List.nth r i
      | Def, i ->
	 fun r ->
	 let Rec x = List.nth r i in
	 let Some v = !x in 
	 v)
  | If (e1, e2, e3) ->
     let c1 = compile_e e1 rn in
     let c2 = compile_e e2 rn in
     let c3 = compile_e e3 rn in
     fun r ->
     (match c1 r with
      | False -> c3 r
      | _ -> c2 r)
  | Match (e0, cls) ->
     let c0 = compile_e e0 rn in
     let cs = compile_cls cls rn in
     fun r ->
     let StructVal (x, vs) = c0 r in
     let rec m cs =
       let (y, c) :: cs = cs in
       if x=y 
       then c ((Array.to_list vs) @ r)
       else m cs
     in
     m cs

  | Fun (xs, e) -> 
     let c = compile_e e ((List.map (fun x -> (Var, x)) xs) @ rn) in
     let i = List.length xs in
     fun r -> CProc (i, c, r)
  | App (e0, []) ->
     let c = compile_e e0 rn in
     fun r -> (match c r with
	       | CProc (0, c, r) -> c r
	       | StructCons (x, 0) -> StructVal (x, [||]))
  | App (e0, [e1]) ->
     let c0 = compile_e e0 rn in
     let c1 = compile_e e1 rn in 
     fun r -> (match ((c0 r), (c1 r)) with
	       | CProc (1, c, r), v -> c (v :: r)
	       | StructCons (x, 1), v -> StructVal (x, [| v |])
	       | StructPred x, StructVal (y, _) -> if x=y then True else False
	       | StructAcc (x, i), StructVal (y, vs) -> vs.(i)
	       | IsCons, Pair _ -> True
	       | IsCons, _ -> False
	       | IsNull, Null -> True
	       | IsNull, _ -> False
	       | Car, Pair (x, y) -> x
	       | Cdr, Pair (x, y) -> y
	       | Sub1, Int i -> Int (pred i)
	       | Add1, Int i -> Int (succ i)
	       | Sqr, Int i -> Int (square i)
	       | List, v -> Pair (v, Null))
  | App (e0, [e1; e2]) ->
     let c0 = compile_e e0 rn in
     let c1 = compile_e e1 rn in 
     let c2 = compile_e e2 rn in 
     fun r -> (match ((c0 r), (c1 r), (c2 r)) with
	       | CProc (2, c, r), v1, v2 -> c (v1 :: v2 :: r)
	       | StructCons (x, 2), v1, v2 -> StructVal (x, [| v1; v2 |])
	       | Cons, v1, v2 -> Pair (v1, v2)
	       | Times, Int i, Int j -> Int (mult i j)
	       | Plus, Int i, Int j -> Int (add i j)
	       | Expt, Int i, Int j -> Int (pow i j)
	       | Equal, Int i, Int j -> if (eq_int i j) then True else False
	       | List, v1, v2 -> Pair (v1, Pair (v2, Null)))
  | App (e, es) ->
     let c = compile_e e rn in
     let cs = compile_es es rn in
     fun r -> apply_c (c r) (cs r)
and compile_es (es : expr list) (rn : nenv) : venv -> value list =
  let cs = List.map (fun e -> compile_e e rn) es in
  fun r -> 
  List.map (fun c -> c r) cs
and compile_cls (cls : ((id * id list) * expr) list) (rn : nenv) : (id * (venv -> value)) list =
  List.map (fun ((x, xs), e) -> (x, compile_e e ((List.map (fun x -> (Var, x)) xs) @ rn))) cls
and apply_c (f : value) (vs : value list) : value =
  match f with
  | CProc (n, c, r) -> c (vs @ r)
  | List  -> (List.fold_right (fun v vs -> Pair (v, vs)) vs Null)
  | StructCons (x, i) -> StructVal (x, Array.of_list vs)


let print_ans (a : value) : unit =
  print_string
    (let rec str_of_ans (a : value) : string =
       (match a with
	| Int i  -> string_of_int i
	| Str s  -> s
	| Null -> "()"
	| Pair (x, y) -> 
	   "(" ^ str_of_ans x ^ " . " ^ str_of_ans y ^ ")"
	| True   -> "#t"
	| False  -> "#f"
	| Proc _
	| Sub1
	| Add1
	| Sqr
	| Plus
	| Times
	| Equal
	| Expt
	| Cons
	| IsCons
	| Car
	| Cdr
	| IsNull
	| List
	| StructPred _
	| StructCons _
	| StructAcc _ -> "<fun>"
	| StructVal (x, vs) ->
	   if Array.length vs = 0
	   then "(" ^ x ^ ")"
	   else
	     "(" ^ x ^ " " 
	     ^ String.concat " " (List.map str_of_ans (Array.to_list vs)) 
	     ^ ")")
     in
     str_of_ans a)   		

let run (p : prog) : value =
  compile_p p n0 v0
  (* for interpreter: *)
  (* eval_p p r0 *)
