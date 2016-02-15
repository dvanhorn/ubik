ubik: lexer.mll parser.mly ast.ml int.cmo eval.ml ubik.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -w -8 -c eval.ml
	ocamlc -c ubik.ml
	ocamlfind ocamlc -o ubik -package zarith zarith.cma lexer.cmo parser.cmo int.cmo eval.cmo ubik.cmo

int.cmo: int.ml
	ocamlfind ocamlc -c -package zarith int.ml

test: dummy
	cd test && make test && ./test

clean:
	-rm ubik *cmi *cmo *~ parser.mli lexer.ml parser.ml oUnit-anon.cache
	cd test && make clean

dummy:

