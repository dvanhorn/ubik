ubik: lexer.mll parser.mly ast.ml int.ml eval.ml ubik.ml
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

test: dummy
	cd test && make test
	test/test

clean:
	-rm ubik *cmi *cmo *~ parser.mli lexer.ml parser.ml oUnit-anon.cache
	cd test && make clean

dummy:

