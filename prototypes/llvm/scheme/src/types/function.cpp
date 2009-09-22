#include <stdio.h>

#include "function.h"

namespace eve {
namespace types {

void Function::Print(const char* original_text, TaggedValue result) const {
  printf("'%s' is <Function @%x>.\n", original_text, result);
}
  
} // namespace types
} // namespace eve