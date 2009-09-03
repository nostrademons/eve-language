#ifndef PARSER_H
#define PARSER_H

#include <stdlib.h>
#include <iostream>
#include <vector>
#include <memory>

#include "expr/expr.h"
#include "expr/literal.h"
#include "expr/funcall.h"

class Parser;

typedef union {
  int num;
  Expr* expr;
  Args* exprList;
  Funcall::OpType op;
} YYSTYPE;

#define YY_EXTRA_TYPE Parser*

#include "parser.tab.h"

int yylex(YYSTYPE* token, YYLTYPE* location, void* scanner);
int yylex_init(void** scanner);
int yylex_destroy(void* scanner);
YY_EXTRA_TYPE yyget_extra(void* scanner);
void yyset_extra(YY_EXTRA_TYPE extra, void* scanner);
int yyparse(void* scanner);
void yyerror(YYLTYPE* location, void* scanner, char const* message);

class Parser {
	
	void* _scanner;
	std::istream* _input;
	Expr* _result;
	
	int read(char* buffer, int max_size) {
		int num_read = _input->readsome(buffer, max_size);
		if (_input->eof()) {
			return 0;	// Technically YY_NULL, but that's defined in lexer.cpp
		} else {
			return num_read;
		}
	}
	
  public:
	explicit Parser() : _input(NULL), _result(NULL) {
		yylex_init(&_scanner);
		yyset_extra(this, _scanner);
	}
	~Parser() {
		yylex_destroy(_scanner);
	}
	friend int yyparse(void* scanner);
	friend int yy_get_next_buffer(void* scanner);

	int parse(std::istream& input) {
		_input = &input;
		if (yyparse(_scanner)) {
			// TODO: real error handling
			std::cerr << "Parse error.\n";
			exit(1);
		}
		return _result->eval();
	}
};

#endif