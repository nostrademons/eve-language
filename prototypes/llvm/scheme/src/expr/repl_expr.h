#ifndef EVE_EXPR_REPL_EXPR_H
#define EVE_EXPR_REPL_EXPR_H

#include <boost/scoped_ptr.hpp>

#include "repl_line.h"

namespace eve {

class Repl;

namespace expr {

class Expr;

class ReplExpr : public ReplLine {
 public:
  ReplExpr(eve::expr::Expr* expr) : expr_(expr) {}
  virtual ReplResult Eval(Repl* repl);

 private:
  boost::scoped_ptr<eve::expr::Expr> expr_;
};

} // namespace expr
} // namespace eve

#endif
