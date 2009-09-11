/* Scanner for simple calculator */

%{
#include <stdlib.h>
#include "parser.h"

#define YY_INPUT(buffer, result, max_size) \
	result = yyget_extra(yyscanner)->read(buffer, max_size);

#define YY_USER_ACTION yylloc->first_line = yylineno;
%}

%array
%option reentrant noyywrap bison-bridge bison-locations yylineno

WORD    [[:alnum:]+\-*/<>!=]

%%

[ \t]+				{}
[[:digit:]]+		{ yylval->num = atoi(yytext); return NUM; }
[[:alnum:]+\-*/]+	{ yylval->sym = strdup(yytext); return SYM; }
"("					{ return LPAREN; }
")"					{ return RPAREN; }

%%

void yyerror(YYLTYPE* location, void* scanner, char const* message) {
	// TODO: real error handling.
	std::cout << message << " at \n";
}