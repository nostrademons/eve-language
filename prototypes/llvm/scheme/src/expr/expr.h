#ifndef EXPR_H
#define EXPR_H

#include <string>
#include <sstream>
#include <vector>

#include "../parser.tab.h"

namespace llvm {
  class IRBuilder;
  class Module;
  class Value;
}

typedef YYLTYPE Location;

class Expr {
 private:
  Location location_;
 public:
  Expr(const Location& location) : location_(location) {}
  virtual ~Expr() {}
  virtual llvm::Value* compile(llvm::Module* module, llvm::IRBuilder* builder) = 0;
  virtual std::string pprint() = 0;
};

#endif