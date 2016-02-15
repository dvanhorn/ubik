test: test.ml
	ocamlfind ocamlc -o test -linkpkg \
		-I .. \
		-package oUnit \
		-package zarith \
		../int.cmo ../eval.cmo ../lexer.cmo ../parser.cmo \
		test.ml

clean:
	-rm *cmi *cmo test oUnit-anon.cache *~

