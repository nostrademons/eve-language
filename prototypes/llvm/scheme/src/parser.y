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

program: expr				{ eve_yyget_extra(scanner)->result_ = $1; }

expr :	  NUM						 { $$ = new eve::expr::IntLiteral(@$, $1); }
		| TRUE						 { $$ = new eve::expr::BoolLiteral(@$, true); }
		| FALSE						 { $$ = new eve::expr::BoolLiteral(@$, false); }
		| LPAREN SYM exprList RPAREN { $$ = new eve::expr::Funcall(@$, $2, $3); free($2); }
		
exprList: /* empty */		{ $$ = new eve::expr::Args(); }
		| exprList expr		{ $1->push_back($2); }
		
%%
