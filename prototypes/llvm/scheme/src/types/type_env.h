#ifndef EVE_TYPES_TYPE_ENV_H
#define EVE_TYPES_TYPE_ENV_H

#include <map>
#include <vector>

#include "bool.h"
#include "function.h"
#include "int.h"

namespace eve {
namespace types { 

class Type;
typedef std::pair<FunctionArgs, Type*> FunctionKey;
typedef std::map<FunctionKey, Function> FunctionMap;

// Dummy class for now; will replace it with something later.
class TypeEnv {
 private:
  Bool bool_;
  Int int_;
  FunctionMap functions_;
  
 public:
  const Bool* GetBool() const { return &bool_; }
  const Int* GetInt() const { return &int_; }
  const Function* GetFunction(FunctionArgs arg_types, Type* return_type) {
    FunctionKey key(arg_types, return_type);
    FunctionMap::const_iterator found(functions_.find(key));
    if (found == functions_.end()) {
      found = functions_.insert(
          std::pair<FunctionKey, Function>(key, Function(arg_types, return_type))).first;
    }
    return &found->second;
  }
};

} // namespace types
} // namespace eve

#endif