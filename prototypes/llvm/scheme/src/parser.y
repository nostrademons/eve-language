%{
#include <stdio.h>
#include <iostream>
#include "parser.h"
%}

%locations
%defines "parser.tab.h"
%define api.pure
%output "parser.c"
%lex-param { void* scanner }
%parse-param { void* scanner }
%token <num> NUM
%token <sym> SYM
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%type <expr> expr
%type <exprList> exprList

%%

program: expr				{ yyget_extra(scanner)->result_ = $1; }

expr :	  NUM						 { $$ = new IntLiteral(@$, $1); }
		| TRUE						 { $$ = new BoolLiteral(@$, true); }
		| FALSE						 { $$ = new BoolLiteral(@$, false); }
		| LPAREN SYM exprList RPAREN { $$ = new Funcall(@$, $2, $3); free($2); }
		
exprList: /* empty */		{ $$ = new Args(); }
		| exprList expr		{ $1->push_back($2); }
		
%%
