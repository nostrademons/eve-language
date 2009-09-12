#ifndef LITERAL_H
#define LITERAL_H

#include "expr.h"

namespace llvm {
  class IRBuilder;
  class Module;
  class Value;
}

class BoolLiteral : public Expr {
  bool value_;
public:
  BoolLiteral(const Location& location, bool value);
  virtual ~BoolLiteral();
  virtual llvm::Value* compile(llvm::Module* module, llvm::IRBuilder* builder);
  virtual std::string pprint();
};

class IntLiteral : public Expr {
	  int value_;
  public:
    IntLiteral(const Location& location, int value);
    virtual ~IntLiteral();
    virtual llvm::Value* compile(llvm::Module* module, llvm::IRBuilder* builder);
    virtual std::string pprint();
};

#endif