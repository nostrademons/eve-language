#include "literal.h"

#include <string>
#include <sstream>

Literal::Literal(int value) : _value(value) {}
Literal::~Literal() {}

int Literal::eval() {
  return _value;
}

std::string Literal::pprint() {
  std::stringstream stream;
  stream << _value;
  return stream.str();
}
