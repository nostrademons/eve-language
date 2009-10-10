#include "repl_assignment.h"

#include "repl_line.h"
#include "../repl.h"
#include "../types/type.h"
#include "../types/type_env.h"

namespace eve {
namespace expr {

ReplResult ReplAssignment::Eval(Repl* repl) {
  eve::types::TypeEnv* typeEnv = repl->GetTypeEnv();
  const eve::types::Type* type = expr_->TypeCheck(typeEnv);
  if (!type) {
    // TODO: rewrite with exceptions.
    typeEnv->PrintErrors();
    return ReplResult(0, NULL);
  }
  eve::types::TaggedValue value = repl->ExecuteCode(*expr_, *type);
  repl->AddBinding(var_, value);
  return ReplResult(value, type);
}

} // namespace expr
} // namespace eve
