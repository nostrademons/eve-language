#ifndef EVE_REPL_H
#define EVE_REPL_H

#include <string>

#include <boost/scoped_ptr.hpp>

#include <llvm/Module.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include "parser.h"

namespace llvm {
class Module;
} // namespace llvm

namespace eve {

class Repl {
 public:
  Repl() : seq_num_(0),
           module_(new llvm::Module("repl")),
           jit_(llvm::ExecutionEngine::create(module_.get())) {}
  std::string EvalOneLine(const std::string& input);
  void StartRepl();

 private:
  int seq_num_;
  eve::Parser parser_;
  boost::scoped_ptr<llvm::Module> module_;
  llvm::ExecutionEngine* jit_;
};

class ReplLine {
  virtual std::string Eval(Repl* repl) = 0;
};

class ReplExpr : public ReplLine {
 public:
  ReplExpr(eve::expr::Expr* expr) : expr_(expr) {}
  virtual std::string Eval(Repl* repl);

 private:
  boost::scoped_ptr<eve::expr::Expr> expr_;
};

class ReplAssignment : public ReplLine {
 public:
  ReplAssignment(const std::string& var, ReplExpr* expr)
      : var_(var), expr_(expr) {}
  virtual std::string Eval(Repl* repl);
 private:
  std::string var_;
  boost::scoped_ptr<ReplExpr> expr_;
};

} // namespace eve

#endif EVE_REPL_H
