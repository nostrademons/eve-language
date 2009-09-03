#ifndef LITERAL_H
#define LITERAL_H

#include <llvm/Value.h>

#include "expr.h"

class Literal : public Expr {
	  int _value;
  public:
    Literal(int value);
    ~Literal();
    virtual int eval();
    virtual llvm::Value* compile();
    virtual std::string pprint();
};

#endif