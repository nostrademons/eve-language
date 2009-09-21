#include "type.h"

#include <llvm/DerivedTypes.h>

namespace eve {
namespace types {
  
const llvm::Type* Type::GetRepresentationType() const {
  return llvm::IntegerType::get(32);
}

llvm::Value* Type::GenerateTaggingCode(
    llvm::IRBuilder* builder, llvm::Value* untagged) const {
  return untagged;
}

llvm::Value* Type::GenerateUntaggingCode(
    llvm::IRBuilder* builder, llvm::Value* tagged) const {
  return tagged;
}

} // namespace types
} // namespace eve