#ifndef EVE_TYPES_TYPE_ENV_H
#define EVE_TYPES_TYPE_ENV_H

#include "bool.h"
#include "int.h"

namespace eve {
namespace types { 

// Dummy class for now; will replace it with something later.
class TypeEnv {
 private:
  Bool bool_;
  Int int_;
  
 public:
  const Bool& GetBool() const { return bool_; }
  const Int& GetInt() const { return int_; }
  
};

} // namespace types
} // namespace eve

#endif