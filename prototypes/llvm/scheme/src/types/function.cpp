#include "function.h"

#include <sstream>

namespace eve {
namespace types {

std::string Function::Print(TaggedValue result) const {
  std::stringstream stream;
  stream << "<Function @" << std::hex << result << ">";
  return stream.str();
}
  
} // namespace types
} // namespace eve
