/* Scanner for simple calculator */

%{
#include <stdlib.h>
#include "parser.h"

extern int yylval;
%}

%option noyywrap

DIGIT 	[0-9]

%%

[ \t]+		{}
{DIGIT}+	{ yylval = atoi(yytext); return NUM; }
"+"			{ return PLUS; }
"-"			{ return MINUS; }
"*"			{ return TIMES; }
"/"			{ return DIVIDE; }

%%

void yy_delete_current_buffer() {
	yy_delete_buffer(YY_CURRENT_BUFFER);
}