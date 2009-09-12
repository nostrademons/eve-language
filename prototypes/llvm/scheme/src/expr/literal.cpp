#include "literal.h"

#include <string>
#include <sstream>

#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>

using llvm::ConstantInt;
using llvm::IntegerType;
using llvm::IRBuilder;
using llvm::Module;
using llvm::Value;

BoolLiteral::BoolLiteral(const Location& location, bool value) : Expr(location), value_(value) {}
BoolLiteral::~BoolLiteral() {}

Value* BoolLiteral::compile(Module* module, IRBuilder* builder) {
  return value_ ? ConstantInt::getTrue() : ConstantInt::getFalse();
}

std::string BoolLiteral::pprint() {
  return value_ ? "True" : "False";
}


IntLiteral::IntLiteral(const Location& location, int value) : Expr(location), value_(value) {}
IntLiteral::~IntLiteral() {}

Value* IntLiteral::compile(Module* module, IRBuilder* builder) {
  return ConstantInt::get(IntegerType::get(32), value_);
}

std::string IntLiteral::pprint() {
  std::stringstream stream;
  stream << value_;
  return stream.str();
}
