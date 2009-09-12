#ifndef LITERAL_H
#define LITERAL_H

#include "expr.h"

namespace llvm {
  class IRBuilder;
  class Module;
  class Value;
}

class IntLiteral : public Expr {
	  int _value;
  public:
    IntLiteral(const Location& location, int value);
    ~IntLiteral();
    virtual llvm::Value* compile(llvm::Module* module, llvm::IRBuilder* builder);
    virtual std::string pprint();
};

#endif