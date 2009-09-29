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

#include "types/type.h"
#include "types/type_env.h"

namespace llvm {
  class Constant;
}

int main (int argc, char const *argv[])
{	
  if(argc <= 1) {
    printf("usage: calculator \"2 + 2\"");
    return -1;
  }
  std::stringstream input(argv[1]);
  eve::Parser parser;
  std::auto_ptr<eve::expr::Expr> expr(parser.Parse("args", input));
  
  eve::types::TypeEnv typeEnv;
  const eve::types::Type* type = expr->TypeCheck(&typeEnv);
  if (!type) {
    typeEnv.PrintErrors();
    return 1;
  }
  
  std::auto_ptr<llvm::Module> module(new llvm::Module("calculator"));
  llvm::Constant* c = module->getOrInsertFunction(
      "calculator", llvm::IntegerType::get(32), NULL);
  llvm::Function* f = llvm::  cast<llvm::Function>(c);
  f->setCallingConv(llvm::CallingConv::C);
  
  llvm::IRBuilder builder(llvm::BasicBlock::Create("entry", f));
  llvm::Value* untaggedResult = expr->Compile(module.get(), &builder);
  builder.CreateRet(type->GenerateTaggingCode(&builder, untaggedResult));
  
  verifyFunction(*f);
  llvm::ExecutionEngine* jit = llvm::ExecutionEngine::create(module.get());
      
  eve::types::TaggedValue (*calculate)() = 
    (eve::types::TaggedValue (*)()) jit->getPointerToFunction(f);
  eve::types::TaggedValue result = calculate();
  type->Print(argv[1], result);
	return 0;
}
