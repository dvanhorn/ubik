let _ =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.main Lexer.token lexbuf in
  let a = Eval.run p in
  Eval.print_ans a;
  print_newline(); flush stdout;
  exit 0
			 
