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
	void* scanner_;
  const char* file_;
	std::istream* input_;
	Expr* result_;
	
	int Read(char* buffer, int max_size) {
		int num_read = input_->readsome(buffer, max_size);
		if (input_->eof()) {
			return 0;	// Technically YY_NULL, but that's defined in lexer.cpp
		} else {
			return num_read;
		}
	}
	
 public:
	explicit Parser() : input_(NULL), result_(NULL) {
		yylex_init(&scanner_);
		yyset_extra(this, scanner_);
	}
	~Parser() {
		yylex_destroy(scanner_);
	}
	friend int yyparse(void* scanner);
	friend int yy_get_next_buffer(void* scanner);

  const char* GetFile() {
    return file_;
  }
  
	Expr* Parse(std::string filename, std::istream& input) {
		input_ = &input;
    file_ = filename.c_str();
		if (yyparse(scanner_)) {
			// TODO: real error handling
			std::cerr << "Parse error.\n";
			exit(1);
		}
		return result_;
	}
};

#endif