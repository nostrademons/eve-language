#include "repl_expr.h"

#include <sstream>

#include "repl_line.h"
#include "../repl.h"
#include "../types/type.h"
#include "../types/type_env.h"

namespace eve {
namespace expr {

using std::stringstream;
using eve::Repl;
using eve::expr::Expr;
using eve::types::TaggedValue;

ReplResult ReplExpr::Eval(Repl* repl) {
  eve::types::TypeEnv* typeEnv = repl->GetTypeEnv();
  const eve::types::Type* type = expr_->TypeCheck(typeEnv);
  if (!type) {
    // TODO: rewrite with exceptions.
    typeEnv->PrintErrors();
    return ReplResult(0, NULL);
  }
  return ReplResult(repl->ExecuteCode(*expr_, *type), type);
}

} // namespace expr
} // namespace eve
