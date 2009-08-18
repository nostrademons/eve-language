%{
#include <stdio.h>

int yylex();
void yy_delete_current_buffer();
int yyreturn;

void yyerror(char const* s) {
	printf("%s\n", s);
}
%}

%locations
%token NUM
%left PLUS MINUS
%left TIMES DIVIDE

%%

program: exp				{ yyreturn = $1; }

exp :	  NUM
		| exp PLUS exp		{ $$ = $1 + $3; }
		| exp MINUS exp		{ $$ = $1 - $3; }
		| exp TIMES exp		{ $$ = $1 * $3; }
		| exp DIVIDE exp	{ $$ = $1 / $3; };
		
%%

int calculate(const char* input) {
	yy_scan_string(input);
	if (yyparse()) {
		printf("Parse error.\n");
	}
	yy_delete_current_buffer();
	return yyreturn;
}