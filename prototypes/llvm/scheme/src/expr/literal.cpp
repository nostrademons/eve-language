#include "literal.h"

#include <string>
#include <sstream>

#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>

#include "../types/bool.h"
#include "../types/int.h"
#include "../types/type_env.h"

namespace eve {
namespace types {
  class Type;
}
  
namespace expr {

using eve::types::Bool;
using eve::types::Int;
using eve::types::Type;
using eve::types::TypeEnv;
using llvm::ConstantInt;
using llvm::IntegerType;
using llvm::IRBuilder;
using llvm::Module;
using llvm::Value;

BoolLiteral::BoolLiteral(const Location& location, bool value) : Expr(location), value_(value) {}
BoolLiteral::~BoolLiteral() {}

const Type* BoolLiteral::TypeCheck(TypeEnv* env) const {
  return env->GetBool();
}

Value* BoolLiteral::compile(Module* module, IRBuilder* builder) const {
  return value_ ? ConstantInt::getTrue() : ConstantInt::getFalse();
}

std::string BoolLiteral::pprint() const {
  return value_ ? "True" : "False";
}


IntLiteral::IntLiteral(const Location& location, int value) : Expr(location), value_(value) {}
IntLiteral::~IntLiteral() {}

const Type* IntLiteral::TypeCheck(TypeEnv* env) const {
  return env->GetInt();
}

Value* IntLiteral::compile(Module* module, IRBuilder* builder) const {
  return ConstantInt::get(IntegerType::get(32), value_);
}

std::string IntLiteral::pprint() const {
  std::stringstream stream;
  stream << value_;
  return stream.str();
}

} // namespace expr
} // namespace eve