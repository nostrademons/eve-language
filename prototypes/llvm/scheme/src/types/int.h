#ifndef EVE_TYPES_INT_H
#define EVE_TYPES_INT_H

#include "type.h"

namespace eve {
namespace types {

class Int : public Type {
 public:
  int Untag(int tagged) { return tagged >> 1; }
   
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
