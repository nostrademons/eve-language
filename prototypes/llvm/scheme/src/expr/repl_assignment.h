#ifndef EVE_EXPR_REPL_ASSIGNMENT_H
#define EVE_EXPR_REPL_ASSIGNMENT_H

#include <string>
#include <boost/scoped_ptr.hpp>

#include "repl_line.h"

namespace eve {

class Repl;

namespace expr {

class Expr;

class ReplAssignment : public ReplLine {
 public:
  ReplAssignment(const std::string& var, Expr* expr)
      : var_(var), expr_(expr) {}
  virtual ReplResult Eval(Repl* repl);
 private:
  std::string var_;
  boost::scoped_ptr<Expr> expr_;
};

} // namespace expr
} // namespace eve

#endif
