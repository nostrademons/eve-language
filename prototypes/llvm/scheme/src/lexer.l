/* Scanner for simple calculator */

%{
#include <stdlib.h>
#include "parser.h"

#define YY_INPUT(buffer, result, max_size) \
	result = eve_yyget_extra(yyscanner)->Read(buffer, max_size);

#define YY_USER_ACTION do { \
	yylloc->Advance(eve_yyget_extra(yyscanner)->GetFile(), yylineno, \
					    yycolumn, yycolumn + yyleng); \
	yycolumn += yyleng; \
  } while(0);
%}

%array
%option reentrant noyywrap yylineno
%option bison-bridge bison-locations
%option outfile="lex.yy.c" prefix="eve_yy"

WORD    [[:alnum:]+\-*/<>!=]

%%

"True"  { return TRUE; }
"False"  { return FALSE; }
"("  { return LPAREN; }
")"  { return RPAREN; }
"="  { return EQ; }
[ \t]+  {}
[[:digit:]]+  { yylval->num = atoi(yytext); return NUM; }
{WORD}+			{ yylval->sym = strdup(yytext); return SYM; }

%%

void eve_yyerror(YYLTYPE* location, void* scanner, char const* message) {
	// TODO: real error handling.
	std::cout << message << " at " << location->ToString() << "\n";
}
