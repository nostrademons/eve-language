#include "repl.h"

#include <string.h>
#include <iostream>
#include <string>

#include <llvm/BasicBlock.h>
#include <llvm/CallingConv.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/Module.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Support/IRBuilder.h>

#include "parser.h"
#include "expr/expr.h"
#include "expr/repl_line.h"
#include "types/type.h"
#include "types/type_env.h"

namespace eve {

using std::cin;
using std::cout;
using std::string;
using boost::scoped_ptr;
using llvm::ExecutionEngine;
using llvm::Function;
using eve::expr::ReplLine;
using eve::expr::ReplResult;
using eve::types::TaggedValue;

void Repl::StartRepl() {
  char input[200];
  cout << ">>> ";
  cin.getline(input, 200);
  while (strcmp(input, "quit")) {
    cout << EvalOneLine(input) << "\n>>> ";
    cin.getline(input, 200);
    seq_num_++;
  }
}

string Repl::EvalOneLine(const string& input) {
  scoped_ptr<ReplLine> line(parser_.ParseRepl(input));
  ReplResult result = line->Eval(this);
  return result.second ? result.second->Print(result.first) : "<error>";
}

TaggedValue Repl::ExecuteCode(const eve::expr::Expr& expr, const eve::types::Type& type) {
  std::stringstream function_name;
  function_name << "repl$" << seq_num_;

  llvm::Constant* c = module_->getOrInsertFunction(
      function_name.str(), llvm::IntegerType::get(32), NULL);
  Function* f = llvm::cast<Function>(c);
  f->setCallingConv(llvm::CallingConv::C);
  
  llvm::IRBuilder builder(llvm::BasicBlock::Create("entry", f));
  llvm::Value* untaggedResult = expr.Compile(module_.get(), &builder);
  builder.CreateRet(type.GenerateTaggingCode(&builder, untaggedResult));
  
  verifyFunction(*f);
      
  TaggedValue (*calc)() = (TaggedValue (*)()) jit_->getPointerToFunction(f);
  return calc();
}

} // namespace eve
