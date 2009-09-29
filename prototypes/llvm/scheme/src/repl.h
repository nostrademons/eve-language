#ifndef EVE_REPL_H
#define EVE_REPL_H

#include <string>

#include <boost/scoped_ptr.hpp>

#include <llvm/Module.h>

#include "parser.h"

namespace llvm {
class Module;
} // namespace llvm

namespace eve {

class Repl {
 public:
  Repl() : seq_num_(0), module_(new llvm::Module("repl")) {}
  std::string EvalOneLine(const std::string& input);
  void StartRepl();

 private:
  int seq_num_;
  eve::Parser parser_;
  boost::scoped_ptr<llvm::Module> module_;
};

} // namespace eve

#endif EVE_REPL_H
