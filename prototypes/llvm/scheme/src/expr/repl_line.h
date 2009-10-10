#ifndef EVE_EXPR_REPL_LINE_H
#define EVE_EXPR_REPL_LINE_H

#include <boost/utility.hpp>

#include "../types/type.h"

namespace eve {

class Repl;
namespace expr {

typedef std::pair<eve::types::TaggedValue, const eve::types::Type*> ReplResult;

class ReplLine : boost::noncopyable {
 public:
  virtual ReplResult Eval(Repl* repl) = 0;
};

} // namespace expr
} // namespace eve

#endif
