#include "literal.h"

#include <string>
#include <sstream>

#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>

using namespace llvm;

Literal::Literal(const Location& location, int value) : Expr(location), _value(value) {}
Literal::~Literal() {}

Value* Literal::compile(Module* module, IRBuilder* builder) {
  return ConstantInt::get(IntegerType::get(32), _value);
}

std::string Literal::pprint() {
  std::stringstream stream;
  stream << _value;
  return stream.str();
}
