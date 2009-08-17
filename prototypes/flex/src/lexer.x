/* Scanner for simple calculator */

%{
#include <stdlib.h>
#include "y.tab.h"
%}

DIGIT 	[0-9]

%%

DIGIT+		{ yylval = atoi(yytext); return TOK_NUMBER; }
"+"			{ return TOK_PLUS; }
"-"			{ return TOK_MINUS; }
"*"			{ return TOK_TIMES; }
"/"			{ return TOK_DIVIDE; }
