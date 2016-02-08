(* File lexer.mll *)
{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

  let keyword_table = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ "lambda", FUN;
		"define", DEF;
		"if", IF;
	      ]

}
rule token = parse
   [' ' '\t' '\n' ]     { token lexbuf }     (* skip whitespace *)
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ['A'-'Z' 'a'-'z' '=' '*' '+'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '?'] * as id 
  { try
      Hashtbl.find keyword_table id
    with Not_found ->
      IDENT id }
| ['#'] ['t' 'T'] { TRUE }
| ['#'] ['f' 'F'] { FALSE }
| '('            { LPAREN }
| ')'            { RPAREN }
| eof            { EOF }
