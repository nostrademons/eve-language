#ifndef EVE_TYPES_BOOL_H
#define EVE_TYPES_BOOL_H

#include "type.h"

namespace eve {
namespace types {

extern const llvm::Type* kLLVMBoolType;

class Bool : public Type {
 public:
  virtual const llvm::Type* GetRepresentationType() const;
  virtual llvm::Value* GenerateTaggingCode(
      llvm::IRBuilder* builder, llvm::Value* untagged) const;
  virtual llvm::Value* GenerateUntaggingCode(
      llvm::IRBuilder* builder, llvm::Value* tagged) const;
  virtual std::string Print(TaggedValue result) const;
};
  
} // namespace types
} // namespace eve

#endif
