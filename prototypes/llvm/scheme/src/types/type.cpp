#include "types.h"

#include <llvm/DerivedTypes.h>

namespace eve {
namespace types {
  
virtual const llvm::Type* Type::GetRepresentationType() const {
  return llvm::IntegerType::get(32);
}

virtual llvm::Value* Type::GenerateTaggingCode(
    llvm::IRBuilder* builder, llvm::Value* untagged) const {
  return untagged;
}

virtual llvm::Value* Type::GenerateUntaggingCode(
    llvm::IRBuilder* builder, llvm::Value* tagged) const {
  return tagged;
}

} // namespace types
} // namespace eve