%{
  open Ast
%}
/* File parser.mly */
%token <int> INT
%token <string> IDENT
%token LPAREN RPAREN
%token FUN DEF IF STRUCT
%token TRUE FALSE
%token EOF
%start main             /* the entry point */
%type <Ast.prog> main
%%
main:
    prog EOF                { $1 }
;
prog:
    expr                      { ([], $1) }
  | defn prog                 
	 { let (ds, e) = $2 in $1 :: ds, e }	 
;
defn:
    LPAREN DEF id expr RPAREN     { Def ($3, $4) }
  | LPAREN DEF defids expr RPAREN 
	   { let (x, xs) = $3 in Def (x, Fun (xs, $4)) }
  | LPAREN STRUCT id ids RPAREN 
	   { Struct ($3, $4) }
;
expr:
    INT                       { Const (Int $1) }
  | TRUE                      { Const True }
  | FALSE                     { Const False }
  | LPAREN IF expr expr expr RPAREN
                              { If ($3, $4, $5) }
  | LPAREN expr exprrest      { App ($2, $3) }
  | id                        { Var $1 }
;
id:
    IDENT                    { $1 }
;
defids:
  | LPAREN id idsrest     { ($2, $3) }
;
exprrest:
  | RPAREN                   { [] }
  | expr exprrest            { $1 :: $2 }
ids:
  | LPAREN idsrest           { $2 }
idsrest:
  | id idsrest               { $1 :: $2 }
  | RPAREN                   { [] }
