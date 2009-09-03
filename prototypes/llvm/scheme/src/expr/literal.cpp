#include "literal.h"

#include <string>
#include <sstream>

#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Value.h>

using llvm::Value;
using llvm::ConstantInt;
using llvm::IntegerType;

Literal::Literal(int value) : _value(value) {}
Literal::~Literal() {}

int Literal::eval() {
  return _value;
}

Value* Literal::compile() {
  return ConstantInt::get(IntegerType::get(32), _value);
}

std::string Literal::pprint() {
  std::stringstream stream;
  stream << _value;
  return stream.str();
}
