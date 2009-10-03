#include <stdio.h>
#include <string>

#include "repl.h"

namespace llvm {
  class Constant;
}

int main (int argc, char const *argv[])
{	
  eve::Repl repl;
  if(argc <= 1) {
    repl.StartRepl();  
  } else {
    std::string result = repl.EvalOneLine(argv[1]);
    printf("%s is %s.\n", argv[1], result.c_str());
  }
  return 0;
}
