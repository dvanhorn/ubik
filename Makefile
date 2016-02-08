default:
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -c nums.cma int.ml
	ocamlc -w -8 -c eval.ml
	ocamlc -c ubik.ml
	ocamlc -o ubik nums.cma lexer.cmo parser.cmo int.cmo eval.cmo ubik.cmo

clean:
	-rm ubik *cmi *cmo *~ parser.mli lexer.ml parser.ml
