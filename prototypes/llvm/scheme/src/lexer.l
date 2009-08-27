/* Scanner for simple calculator */

%{
#include <stdlib.h>
#include "parser.h"

#define YY_INPUT(buffer, result, max_size) \
	result = yyget_extra(yyscanner)->read(buffer, max_size);

#define YY_USER_ACTION yylloc->first_line = yylineno;
%}

%option reentrant noyywrap bison-bridge bison-locations yylineno

DIGIT 	[0-9]

%%

[ \t]+		{}
{DIGIT}+	{ *yylval = atoi(yytext); return NUM; }
"+"			{ return PLUS; }
"-"			{ return MINUS; }
"*"			{ return TIMES; }
"/"			{ return DIVIDE; }
"("			{ return LPAREN; }
")"			{ return RPAREN; }

%%

void yyerror(YYLTYPE* location, void* scanner, char const* message) {
	// TODO: real error handling.
	std::cout << message << "\n";
}