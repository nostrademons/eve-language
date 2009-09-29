#include "int.h"

#include <sstream>

#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Support/IRBuilder.h>

namespace eve {
namespace types {

const llvm::Type* Int::GetRepresentationType() const {
  // TODO: 64-bit compatibility?
  return llvm::IntegerType::get(32);
}

llvm::Value* Int::GenerateTaggingCode(
    llvm::IRBuilder* builder, llvm::Value* untagged) const {
  llvm::Value* shifted = builder->CreateShl(untagged,
      llvm::ConstantInt::get(GetRepresentationType(), 1));
  return builder->CreateOr(shifted,
      llvm::ConstantInt::get(GetRepresentationType(), 1));
}
                                 
llvm::Value* Int::GenerateUntaggingCode(
    llvm::IRBuilder* builder, llvm::Value* tagged) const {
  return builder->CreateLShr(tagged,
      llvm::ConstantInt::get(GetRepresentationType(), 1));
}

std::string Int::Print(TaggedValue result) const {
  std::stringstream stream;
  stream << (result >> 1);
  return stream.str();
}

} // namespace types
} // namespace eve
