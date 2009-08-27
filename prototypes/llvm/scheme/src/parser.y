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
%token NUM
%token LPAREN
%token RPAREN
%left PLUS MINUS
%left TIMES DIVIDE

%%

program: exp				{ yyget_extra(scanner)->_result = $1; }

exp :	  NUM
		| exp PLUS exp		{ $$ = $1 + $3; }
		| exp MINUS exp		{ $$ = $1 - $3; }
		| exp TIMES exp		{ $$ = $1 * $3; }
		| exp DIVIDE exp	{ $$ = $1 / $3; }
		| LPAREN exp RPAREN { $$ = $2; 		}
		
%%

