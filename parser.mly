%{
  open Ast
%}
/* File parser.mly */
%token <int> INT
%token <string> IDENT
%token LPAREN RPAREN
%token FUN DEF IF
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
    LPAREN DEF id expr RPAREN     { ($3, $4) }
  | LPAREN DEF defids expr RPAREN 
	   { let (x, xs) = $3 in (x, Fun (xs, $4)) }
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
  | LPAREN id defidsrest     { ($2, $3) }
;
defidsrest:
  | RPAREN                   { [] }
  | id defidsrest            { $1 :: $2 }
exprrest:
  | RPAREN                   { [] }
  | expr exprrest            { $1 :: $2 }
