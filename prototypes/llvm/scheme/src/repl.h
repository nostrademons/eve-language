#ifndef EVE_REPL_H
#define EVE_REPL_H

#include <string>

#include <boost/scoped_ptr.hpp>
#include <boost/utility.hpp>

#include <llvm/Module.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include "parser.h"
#include "expr/expr.h"
#include "expr/repl_line.h"
#include "types/type.h"
#include "types/type_env.h"

namespace llvm {
class Module;
} // namespace llvm

namespace eve {

class Repl : boost::noncopyable {
 public:
  Repl() : seq_num_(0),
           module_(new llvm::Module("repl")),
           jit_(llvm::ExecutionEngine::create(module_.get())) {}
  void StartRepl();
  std::string EvalOneLine(const std::string& input);

  eve::types::TypeEnv* GetTypeEnv() { return &type_env_; }
  eve::types::TaggedValue ExecuteCode(const eve::expr::Expr& expr,
                                      const eve::types::Type& type);
  void AddBinding(const std::string& var, eve::types::TaggedValue value) {
    symbol_table_[var] = value;
  }

 private:
  int seq_num_;
  eve::Parser parser_;
  eve::types::TypeEnv type_env_;
  std::map<std::string, eve::types::TaggedValue> symbol_table_;
  boost::scoped_ptr<llvm::Module> module_;
  llvm::ExecutionEngine* jit_;
};

} // namespace eve

#endif EVE_REPL_H
