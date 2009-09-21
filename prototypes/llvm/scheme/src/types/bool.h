#ifndef EVE_TYPES_BOOL_H
#define EVE_TYPES_BOOL_H

#include "type.h"

namespace llvm {
class Type;
class Value;
} // namespace llvm

namespace eve {
namespace types {

class Bool : public Type {
 public:
  virtual const llvm::Type* GetRepresentationType() const;
  virtual llvm::Value* GenerateTaggingCode(
      llvm::IRBuilder* builder, llvm::Value* untagged) const;
  virtual llvm::Value* GenerateUntaggingCode(
      llvm::IRBuilder* builder, llvm::Value* tagged) const;
  virtual void Print(const char* original_text, TaggedValue result) const;
};
  
} // namespace types
} // namespace eve

#endif