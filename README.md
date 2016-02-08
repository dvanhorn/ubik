# Ubik

A small Scheme interpreter and compiler, written in OCaml, for
illustrative purposes.

Programs are of the form:

```
<prog> ::= <defn>* <expr>
<defn> ::= (define <id> <expr>)
        |  (define (<id>+) <expr>)
        |  (struct <id> (<id>*))
<expr> ::= <int>
        |  #t
        |  #f
        |  <id>
        |  (<expr>+)
        |  (lambda (<id>+) <expr>)
        |  (if <expr> <expr> <expr>)
        |  (match <expr> (<pat> <expr>)*)
<pat>  ::= (<id>+)
```

The implementation supports: arbitrary precision integers,
higher-order functions, structures, simple pattern matching, some
arithmetic operations (+, add1, sub1, =, expt), and list operations
(list, cons, car, cdr, null, null?, pair?).

To build and run:

```
   make
   ./ubik
```
