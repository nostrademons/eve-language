#include <memory>
#include <stdio.h>
#include <sstream>
#include "parser.h"

#include <llvm/BasicBlock.h>
#include <llvm/CallingConv.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/Module.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Support/IRBuilder.h>

namespace llvm {
  class Constant;
}

using namespace llvm;
using namespace std;

int main (int argc, char const *argv[])
{	
	if(argc <= 1) {
		printf("usage: calculator \"2 + 2\"");
		return -1;
	}
	stringstream input(argv[1]);
	Parser parser;
  auto_ptr<Expr> expr(parser.parse(input));
  auto_ptr<Module> module(new Module("calculator"));
  Constant* c = module->getOrInsertFunction("calculator", IntegerType::get(32), NULL);
  Function* f = cast<Function>(c);
  f->setCallingConv(CallingConv::C);
  
  IRBuilder builder(BasicBlock::Create("entry", f));
  builder.CreateRet(expr->compile(module.get(), &builder));
  
  verifyFunction(*f);
  ExecutionEngine* jit = ExecutionEngine::create(module.get());
      
  int (*calculate)() = (int (*)()) jit->getPointerToFunction(f);
	printf("'%s' is %d.\n", argv[1], calculate());
	return 0;
}