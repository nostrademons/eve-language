#ifndef EXPR_H
#define EXPR_H

#include <string>
#include <sstream>
#include <vector>

#include <llvm/Value.h>

class Expr {
  public:
    Expr() {}
	  virtual ~Expr() {}
	  virtual int eval() = 0;
    virtual llvm::Value* compile() = 0;
	  virtual std::string pprint() = 0;
};

#endif