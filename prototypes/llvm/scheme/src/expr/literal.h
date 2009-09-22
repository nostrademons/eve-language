#ifndef LITERAL_H
#define LITERAL_H

#include "expr.h"

namespace llvm {
  class IRBuilder;
  class Module;
  class Value;
}

namespace eve {
  namespace types {
    class Type;
    class TypeEnv;
  }
  
  namespace expr {

    class BoolLiteral : public Expr {
      bool value_;
    public:
      BoolLiteral(const Location& location, bool value);
      virtual ~BoolLiteral();
      virtual const eve::types::Type* TypeCheck(eve::types::TypeEnv* env) const;
      virtual llvm::Value* compile(llvm::Module* module, llvm::IRBuilder* builder) const;
      virtual std::string pprint() const;
    };

    class IntLiteral : public Expr {
    	  int value_;
      public:
        IntLiteral(const Location& location, int value);
        virtual ~IntLiteral();
        virtual const eve::types::Type* TypeCheck(eve::types::TypeEnv* env) const;
        virtual llvm::Value* compile(llvm::Module* module, llvm::IRBuilder* builder) const;
        virtual std::string pprint() const;
    };

  } // namespace expr
} // namespace eve

#endif