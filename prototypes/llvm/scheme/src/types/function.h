#ifndef EVE_TYPES_FUNCTION_H
#define EVE_TYPES_FUNCTION_H

#include <vector>

#include "type.h"

namespace eve {
namespace types {

class TypeEnv;
typedef std::vector<const Type*> FunctionArgs;

class Function : public Type {
 private:
  FunctionArgs arg_types_;
  const Type* return_type_;
  
 public:
  Function(const FunctionArgs& arg_types, const Type* return_type)
       : arg_types_(arg_types), return_type_(return_type) {}
  
  const FunctionArgs& GetArgTypes() const { return arg_types_; }
  const Type* GetReturnType() const { return return_type_; }

  // TODO: Temporary; going away when str() is implemented.
  virtual void Print(const char* original_text, TaggedValue result) const;
};

}
}

#endif