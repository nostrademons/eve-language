#ifndef EXPR_H
#define EXPR_H

#include <string>
#include <sstream>
#include <vector>

#include <boost/utility.hpp>

#include "../location.h"

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

    class Expr : boost::noncopyable {
     private:
      Location location_;
      
     protected:
      const Location& GetLocation() const { return location_; }
      
     public:
      Expr(const Location& location) : location_(location) {}
      virtual ~Expr() {}
      virtual const eve::types::Type* TypeCheck(eve::types::TypeEnv* env) const = 0;
      virtual llvm::Value* Compile(llvm::Module* module, llvm::IRBuilder* builder) const = 0;
      virtual std::string PPrint() const = 0;
    };

  }
} // namespace eve

#endif
