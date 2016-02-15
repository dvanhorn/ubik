open OUnit

let run_s (s : string) : Eval.value =
  let lexbuf = Lexing.from_string s in
  let p = Parser.main Lexer.token lexbuf in
  Eval.run p

let run_f (f : string) : Eval.value =
  let c = open_in ("../progs/" ^ f) in
  let lexbuf = Lexing.from_channel c in
  let p = Parser.main Lexer.token lexbuf in
  let a = Eval.run p in
  close_in c; a 

let five = Eval.Int (Int.int_of 5)

let test_const () = 
  assert_equal five (run_s "5")

let test_id () =
  assert_equal five (run_s "((lambda (x) x) 5)")

let test_add1 () =
  assert_equal five (run_s "(add1 4)")

let test_double () =
  assert_equal five (run_s "((lambda (f) (f (f 3))) add1)")

let test_def_id () =
  assert_equal five (run_s "(define (id x) x) (id 5)")

let test_fact5 () =
  assert_equal (Eval.Int (Int.int_of 120))
    (run_f "fact5.scm")

let test_expt () =
  assert_equal (Eval.Int (Z.pow (Z.of_int 2) 65536))
    (run_f "expt.scm")

let test_ack () =
  assert_equal (Eval.Int (Int.int_of 1021))
    (run_f "ack.scm")

let suite_eval = "Eval" >::: 
  ["const">:: test_const;
   "id">:: test_id;
   "add1">:: test_add1;
   "double">:: test_double;
   "defid">:: test_def_id;
   "fact5">:: test_fact5;
   "expt">:: test_expt;
   "ack">:: test_ack]

let _ = Printf.printf "Eval:\n"
let _ = run_test_tt_main suite_eval

