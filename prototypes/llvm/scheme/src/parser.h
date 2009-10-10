#ifndef EVE_PARSER_H
#define EVE_PARSER_H

#include <stdlib.h>
#include <iostream>
#include <vector>

#include <boost/scoped_ptr.hpp>
#include <boost/utility.hpp>

#include "location.h"
#include "expr/expr.h"
#include "expr/literal.h"
#include "expr/funcall.h"
#include "expr/repl_expr.h"
#include "expr/repl_assignment.h"

namespace eve {
  class Parser;
}

typedef union {
  int num;
  char* sym;
  eve::expr::ReplLine* replLine;
  eve::expr::Expr* expr;
  eve::expr::Args* exprList;
} YYSTYPE;

typedef eve::Location YYLTYPE;
#define YYLTYPE_IS_DECLARED 1

#define YYLLOC_DEFAULT(current, rhs, n) do { \
  if (n) { \
    (current).MergeFrom(YYRHSLOC(rhs, 1), YYRHSLOC(rhs, n)); \
  } else { \
    (current).SetAfter(YYRHSLOC(rhs, 0)); \
  } \
} while(0);

#define YY_EXTRA_TYPE eve::Parser*

#include "parser.tab.h"

int eve_yylex(YYSTYPE* token, YYLTYPE* location, void* scanner);
int eve_yylex_init(void** scanner);
int eve_yylex_destroy(void* scanner);
YY_EXTRA_TYPE eve_yyget_extra(void* scanner);
void eve_yyset_extra(YY_EXTRA_TYPE extra, void* scanner);
int eve_yyparse(void* scanner);
void eve_yyerror(YYLTYPE* location, void* scanner, char const* message);
int eve_yy_get_next_buffer(void* scanner);

namespace eve {

class Parser : boost::noncopyable {
 public:
	explicit Parser() : input_(NULL), repl_result_(NULL) {
		eve_yylex_init(&scanner_);
		eve_yyset_extra(this, scanner_);
	}
	~Parser() {
		eve_yylex_destroy(scanner_);
	}

	int Read(char* buffer, int max_size) {
		int num_read = input_->readsome(buffer, max_size);
		if (input_->eof()) {
			return 0;	// Technically YY_NULL, but that's defined in lexer.cpp
		} else {
			return num_read;
		}
	}

  const char* GetFile() {
    return file_;
  }
  
	eve::expr::ReplLine* ParseRepl(const std::string& input) {
		input_.reset(new std::stringstream(input));
    file_ = "repl";
		if (eve_yyparse(scanner_)) {
			// TODO: real error handling
			std::cerr << "Parse error.\n";
			exit(1);
		}
		return repl_result_;
	}

	friend int ::eve_yyparse(void* scanner);
	friend int ::eve_yy_get_next_buffer(void* scanner);

 private:
	void* scanner_;
  const char* file_;
	boost::scoped_ptr<std::istream> input_;
	eve::expr::ReplLine* repl_result_;
};

} // namespace eve

#endif
