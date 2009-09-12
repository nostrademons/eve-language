#include "literal.h"

#include <string>
#include <sstream>

#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>

using namespace llvm;

IntLiteral::IntLiteral(const Location& location, int value) : Expr(location), _value(value) {}
IntLiteral::~IntLiteral() {}

Value* IntLiteral::compile(Module* module, IRBuilder* builder) {
  return ConstantInt::get(IntegerType::get(32), _value);
}

std::string IntLiteral::pprint() {
  std::stringstream stream;
  stream << _value;
  return stream.str();
}
