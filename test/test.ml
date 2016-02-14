open OUnit
open Eval
open Ast
open Int

let run_e e = eval_p ([], e) []

let test_const () = 
  assert_equal (Eval.Int (int_of 5))
    (run_e (Const (Int 5)))

let test_id () =
  assert_equal (Eval.Int (int_of 5))
    (run_e (App (Fun (["x"], Var "x"),
		 [Const (Int 5)])))
	      

let suite_eval = "Eval" >::: 
  ["const">:: test_const]

let _ = Printf.printf "Eval:\n"
let _ = run_test_tt_main suite_eval

