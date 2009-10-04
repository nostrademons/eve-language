#ifndef EVE_TYPES_TYPE_ENV_H
#define EVE_TYPES_TYPE_ENV_H

#include <iostream>
#include <map>
#include <string>
#include <vector>

#include <boost/utility.hpp>

#include "bool.h"
#include "function.h"
#include "int.h"

#include "../location.h"

namespace eve {
namespace types {

using eve::Location;

class Type;
typedef std::pair<FunctionArgs, const Type*> FunctionKey;
typedef std::map<FunctionKey, Function*> FunctionMap;
typedef std::vector<std::string> ErrorList;

// Dummy class for now; will replace it with something later.
class TypeEnv : boost::noncopyable {
 private:
  Bool bool_;
  Int int_;
  FunctionMap functions_;
  
  ErrorList errors_;
  
 public:
  ~TypeEnv() {
    for (FunctionMap::const_iterator i = functions_.begin(); i != functions_.end(); ++i) {
      delete i->second;
    }
  }

  const Bool* GetBool() const { return &bool_; }
  const Int* GetInt() const { return &int_; }
  const Function* GetFunction(const FunctionArgs arg_types, const Type* return_type) {
    FunctionKey key(arg_types, return_type);
    FunctionMap::const_iterator found(functions_.find(key));
    if (found == functions_.end()) {
      found = functions_.insert(
          FunctionMap::value_type(key, new Function(arg_types, return_type))).first;
    }
    return found->second;
  }
  
  void AddError(const Location& location, const std::string& error) {
    errors_.push_back(error + " @" + location.ToString());
  }
  void PrintErrors() const {
    for (ErrorList::const_iterator i = errors_.begin(), e = errors_.end();
         i != e; ++i) {
      std::cout << *i << "\n";
    }
  }
};

} // namespace types
} // namespace eve

#endif
