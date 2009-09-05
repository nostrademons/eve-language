#ifndef LITERAL_H
#define LITERAL_H

#include "expr.h"

namespace llvm {
  class IRBuilder;
  class Module;
  class Value;
}

class Literal : public Expr {
	  int _value;
  public:
    Literal(const Location& location, int value);
    ~Literal();
    virtual int eval();
    virtual llvm::Value* compile(llvm::Module* module, llvm::IRBuilder* builder);
    virtual std::string pprint();
};

#endif