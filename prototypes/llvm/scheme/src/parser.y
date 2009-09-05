%{
#include <stdio.h>
#include "parser.h"
%}

%locations
%defines "parser.tab.h"
%define api.pure
%output "parser.c"
%lex-param { void* scanner }
%parse-param { void* scanner }
%token <num> NUM
%token LPAREN
%token RPAREN
%left PLUS MINUS
%left TIMES DIVIDE
%type <expr> expr
%type <exprList> exprList
%type <op> op

%%

program: expr				{ yyget_extra(scanner)->_result = $1; }

expr :	  NUM						{ $$ = new Literal(@$, $1); }
		| LPAREN op exprList RPAREN { $$ = new Funcall(@$, $2, $3); }
		
exprList: /* empty */		{ $$ = new Args(); }
		| exprList expr		{ $1->push_back($2); }
		
op :	  PLUS				{ $$ = Funcall::OpPlus; }
		| MINUS				{ $$ = Funcall::OpMinus; }
		| TIMES				{ $$ = Funcall::OpTimes; }
		| DIVIDE			{ $$ = Funcall::OpDivide; }

%%

