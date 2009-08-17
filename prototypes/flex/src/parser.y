%{
int yylex();
%}

%locations
%token NUMBER
%left PLUS MINUS
%left TIMES DIVIDE

%%

exp :	  NUM
		| exp PLUS exp		{ $$ = $1 + $3; }
		| exp MINUS exp		{ $$ = $1 - $3; }
		| exp TIMES exp		{ $$ = $1 * $3; }
		| exp DIVIDE exp	{ $$ = $1 / $3; }
		
%%

int calculate(char* input) {
	yy_scan_string(input);
	yyparse();
	yy_delete_buffer(YY_CURRENT_BUFFER);
}