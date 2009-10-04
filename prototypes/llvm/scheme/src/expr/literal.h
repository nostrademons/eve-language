#ifndef LITERAL_H
#define LITERAL_H

#include "expr.h"

namespace llvm {
class IRBuilder;
class Module;
class Value;
} // namespace llvm

namespace eve {
namespace types {
  class Type;
  class TypeEnv;
} // namespace types

namespace expr {

class BoolLiteral : public Expr {
 public:
  BoolLiteral(const Location& location, bool value);
  virtual ~BoolLiteral();
  virtual const eve::types::Type* TypeCheck(eve::types::TypeEnv* env) const;
  virtual llvm::Value* Compile(llvm::Module* module, llvm::IRBuilder* builder) const;
  virtual std::string PPrint() const;

 private:
  bool value_;
};

class IntLiteral : public Expr {
 public:
  IntLiteral(const Location& location, int value);
  virtual ~IntLiteral();
  virtual const eve::types::Type* TypeCheck(eve::types::TypeEnv* env) const;
  virtual llvm::Value* Compile(llvm::Module* module, llvm::IRBuilder* builder) const;
  virtual std::string PPrint() const;

 private:
  int value_;
};

} // namespace expr
} // namespace eve

#endif
