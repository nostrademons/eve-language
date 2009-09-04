#ifndef LITERAL_H
#define LITERAL_H

#include <memory>
#include <string>

#include "expr.h"

namespace llvm {
  class IRBuilder;
  class Module;
  class Value;
}

class Literal : public Expr {
	  int _value;
  public:
    Literal(int value);
    ~Literal();
    virtual int eval();
    virtual std::auto_ptr<llvm::Value>
        compile(llvm::Module& module, llvm::IRBuilder& builder);
    virtual std::string pprint();
};

#endif