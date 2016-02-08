%{
  open Ast
%}
/* File parser.mly */
%token <int> INT
%token <string> IDENT
%token LPAREN RPAREN
%token FUN DEF IF STRUCT MATCH
%token TRUE FALSE
%token EOF
%start main             /* the entry point */
%type <Ast.prog> main
%%
main:
  | prog EOF                  { $1 }
  ;
prog:
  | expr                      { ([], $1) }
  | defn prog                 
	 { let (ds, e) = $2 in $1 :: ds, e }	 
  ;
defn:
  | LPAREN DEF id expr RPAREN { Def ($3, $4) }
  | LPAREN DEF defidlist expr RPAREN 
	   { let (x, xs) = $3 in Def (x, Fun (xs, $4)) }
  | LPAREN STRUCT id idlist RPAREN 
	   { Struct ($3, $4) }
  ;
expr:
    INT                       { Const (Int $1) }
  | TRUE                      { Const True }
  | FALSE                     { Const False }
  | LPAREN IF expr expr expr RPAREN
                              { If ($3, $4, $5) }
  | LPAREN expr exprs RPAREN  { App ($2, $3) }
  | LPAREN MATCH expr clauses RPAREN
	   { Match ($3, $4) }
  | id                        { Var $1 }
  ;
id:
  | IDENT                     { $1 }
  ;
defidlist:
  | LPAREN id ids RPAREN      { ($2, $3) }
  ;
exprs:
  |                           { [] }
  | expr exprs                { $1 :: $2 }
idlist:
  | LPAREN ids RPAREN         { $2 }
  ;
clauses:
  |                           { [] }
  | clause clauses            { $1 :: $2 }
  ;
clause:
  | LPAREN pat expr RPAREN    { ($2, $3) }
  ;
pat:
  | LPAREN id ids RPAREN      { $2, $3 }
  ;
ids:
  |                           { [] }
  | id ids                    { $1 :: $2 }
  ;
