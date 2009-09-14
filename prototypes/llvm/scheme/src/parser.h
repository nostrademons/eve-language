#ifndef PARSER_H
#define PARSER_H

#include <stdlib.h>
#include <iostream>
#include <vector>
#include <memory>

#include "location.h"
#include "expr/expr.h"
#include "expr/literal.h"
#include "expr/funcall.h"

class Parser;

typedef union {
  int num;
  char* sym;
  Expr* expr;
  Args* exprList;
} YYSTYPE;

typedef Location YYLTYPE;
#define YYLTYPE_IS_DECLARED 1

#define YYLLOC_DEFAULT(current, rhs, n) do { \
  if (n) { \
    (current).MergeFrom(YYRHSLOC(rhs, 1), YYRHSLOC(rhs, n)); \
  } else { \
    (current).SetAfter(YYRHSLOC(rhs, 0)); \
  } \
} while(0);

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
 private:
	void* _scanner;
  const char* file_;
	std::istream* _input;
	Expr* _result;
	
	int Read(char* buffer, int max_size) {
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

  const char* GetFile() {
    return file_;
  }
  
	Expr* parse(std::string filename, std::istream& input) {
		_input = &input;
    file_ = filename.c_str();
		if (yyparse(_scanner)) {
			// TODO: real error handling
			std::cerr << "Parse error.\n";
			exit(1);
		}
		return _result;
	}
};

#endif