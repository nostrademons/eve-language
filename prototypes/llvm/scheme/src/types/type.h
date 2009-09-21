#ifndef EVE_TYPES_TYPE_H
#define EVE_TYPES_TYPE_H

namespace llvm {
class IRBuilder;
class Type;
class Value;
} // namespace llvm

namespace eve {
namespace types {

// TODO: This probably differs on 64-bit machines.
typedef int TaggedValue;

class Type {
 public:
  virtual const llvm::Type* GetRepresentationType() const;
  virtual llvm::Value* GenerateTaggingCode(
      llvm::IRBuilder* builder, llvm::Value* untagged) const;
  virtual llvm::Value* GenerateUntaggingCode(
      llvm::IRBuilder* builder, llvm::Value* tagged) const;
  // TODO: Temporary; going away when str() is implemented.
  virtual void Print(const char* original_text, TaggedValue result) const = 0;
};
  
} // namespace types
} // namespace eve

#endif