#include <stdio.h>
#include <string>

#include "repl.h"

namespace llvm {
  class Constant;
}

int main (int argc, char const *argv[])
{	
  if(argc <= 1) {
    printf("usage: calculator \"2 + 2\"");
    return -1;
  }
  eve::Repl repl;
  std::string result = repl.EvalOneLine(argv[1]);
  printf("%s is %s.\n", argv[1], result.c_str());
}
