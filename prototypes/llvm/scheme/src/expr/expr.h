#ifndef EXPR_H
#define EXPR_H

#include <memory>
#include <string>
#include <sstream>
#include <vector>

namespace llvm {
  class IRBuilder;
  class Module;
  class Value;
}

class Expr {
  public:
    Expr() {}
	  virtual ~Expr() {}
	  virtual int eval() = 0;
    virtual std::auto_ptr<llvm::Value>
        compile(llvm::Module& module, llvm::IRBuilder& builder) = 0;
	  virtual std::string pprint() = 0;
};

#endif