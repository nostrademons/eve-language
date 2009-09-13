/* Scanner for simple calculator */

%{
#include <stdlib.h>
#include "parser.h"

#define YY_INPUT(buffer, result, max_size) \
	result = yyget_extra(yyscanner)->read(buffer, max_size);

#define YY_USER_ACTION do { \
	yylloc->first_line = yylineno; \
	yylloc->first_column = yycolumn; \
	yycolumn += yyleng; \
  } while(0);
%}

%array
%option reentrant noyywrap bison-bridge bison-locations yylineno

WORD    [[:alnum:]+\-*/<>!=]

%%

"True"				{ return TRUE; }
"False"				{ return FALSE; }
"("					{ return LPAREN; }
")"					{ return RPAREN; }
[ \t]+				{}
[[:digit:]]+		{ yylval->num = atoi(yytext); return NUM; }
[[:alnum:]+\-*/]+	{ yylval->sym = strdup(yytext); return SYM; }

%%

void yyerror(YYLTYPE* location, void* scanner, char const* message) {
	// TODO: real error handling.
	std::cout << message << " at " << location->first_line 
				<< "." << location->first_column << "\n";
}