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
        virtual eve::types::Type* TypeCheck(eve::types::TypeEnv* env);
        virtual llvm::Value* compile(llvm::Module* module, llvm::IRBuilder* builder);
        virtual std::string pprint();
    };

  } // namespace expr
} // namespace eve

#endif