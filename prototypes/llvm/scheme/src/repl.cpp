#include "repl.h"

#include <sstream>
#include <string>

#include <llvm/BasicBlock.h>
#include <llvm/CallingConv.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/Module.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Support/IRBuilder.h>

#include "parser.h"
#include "expr/expr.h"
#include "types/type.h"
#include "types/type_env.h"

namespace eve {

using std::string;
using std::stringstream;
using boost::scoped_ptr;
using llvm::Function;
using llvm::ExecutionEngine;
using eve::expr::Expr;
using eve::types::TaggedValue;

string Repl::EvalOneLine(const string& input) {
  stringstream in_stream(input);
  scoped_ptr<Expr> expr(parser_.Parse("repl", in_stream));

  eve::types::TypeEnv typeEnv;
  const eve::types::Type* type = expr->TypeCheck(&typeEnv);
  if (!type) {
    typeEnv.PrintErrors();
    return "<error>";
  }
  
  stringstream function_name;
  function_name << "repl$" << seq_num_;

  llvm::Constant* c = module_->getOrInsertFunction(
      function_name.str(), llvm::IntegerType::get(32), NULL);
  Function* f = llvm::cast<Function>(c);
  f->setCallingConv(llvm::CallingConv::C);
  
  llvm::IRBuilder builder(llvm::BasicBlock::Create("entry", f));
  llvm::Value* untaggedResult = expr->Compile(module_.get(), &builder);
  builder.CreateRet(type->GenerateTaggingCode(&builder, untaggedResult));
  
  verifyFunction(*f);
  ExecutionEngine* jit = ExecutionEngine::create(module_.get());
      
  TaggedValue (*calculate)() = (TaggedValue (*)()) jit->getPointerToFunction(f);
  TaggedValue result = calculate();
  return type->Print(result);
}

} // namespace eve
