#ifndef FUNCALL_H
#define FUNCALL_H

#include <string>
#include <vector>

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

  typedef std::vector<Expr*> Args;

    class Funcall : public Expr {
      private:
        std::string op_;
    	  Args* args_;
      public:
        Funcall(const Location& location, const char* op, Args* args);
        virtual ~Funcall();
        virtual const eve::types::Type* TypeCheck(eve::types::TypeEnv* env) const;
        virtual llvm::Value* Compile(
            llvm::Module* module, llvm::IRBuilder* builder) const;
        virtual std::string PPrint() const;
    };

  } // namespace expr
} // namespace eve

#endif