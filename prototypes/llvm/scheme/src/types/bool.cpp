#include "bool.h"

#include "llvm/DerivedTypes.h"
#include "llvm/Support/IRBuilder.h"

namespace eve {
namespace types {

const llvm::Type* kLLVMBoolType = llvm::IntegerType::get(1);

const llvm::Type* Bool::GetRepresentationType() const {
  return kLLVMBoolType;
}

llvm::Value* Bool::GenerateTaggingCode(
    llvm::IRBuilder* builder, llvm::Value* untagged) const {
  // TODO: Make a constant for this so it can be changed on 64-bit machines.
  return builder->CreateZExt(untagged, llvm::IntegerType::get(32));
}
                                 
llvm::Value* Bool::GenerateUntaggingCode(
    llvm::IRBuilder* builder, llvm::Value* tagged) const {
  return builder->CreateTrunc(tagged, GetRepresentationType());
}

std::string Bool::Print(TaggedValue result) const {
  return result ? "True" : "False";
}

} // namespace types
} // namespace eve
