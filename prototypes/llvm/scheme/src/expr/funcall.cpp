#include "funcall.h"

#include <assert.h>
#include <iostream>
#include <map>
#include <sstream>
#include <string>

#include <llvm/Instructions.h>
#include <llvm/Support/IRBuilder.h>

#include "../types/bool.h"
#include "../types/function.h"
#include "../types/int.h"
#include "../types/type_env.h"

namespace eve {
namespace types {
  class Type;
}

namespace expr {

using eve::types::Function;
using eve::types::FunctionArgs;
using eve::types::Int;
using eve::types::Type;
using eve::types::TypeEnv;

using llvm::BasicBlock;
using llvm::ICmpInst;
using llvm::IRBuilder;
using llvm::Module;
using llvm::PHINode;
using llvm::Value;

using std::string;

class Primitive;
static std::map<string, Primitive*> primitives;

class Primitive {
 public:
  virtual const Function* GetType(TypeEnv* env) const = 0;
  virtual Value* Compile(Module* module, IRBuilder* builder, Args* args) const = 0;
  virtual const string& PPrint() const { return name_; }
 protected: 
  Primitive(const string& name) : name_(name) {
    primitives[name] = this;
  }
 private:
  string name_;
};

class Binop : public Primitive {
 public:
  virtual const Function* GetType(TypeEnv* env) const {
    FunctionArgs args;
    args.push_back(GetArgType(env));
    args.push_back(GetArgType(env));
    return env->GetFunction(args, GetReturnType(env));
  }
  virtual Value* Compile(Module* module, IRBuilder* builder, Args* args) const {
    Value* first_arg = (*args)[0]->Compile(module, builder);
    Value* second_arg = (*args)[1]->Compile(module, builder);
    return CompilePair(builder, first_arg, second_arg);
  }
 protected:
  Binop(const string& name) : Primitive(name) {}
 private:
  virtual Value* CompilePair(IRBuilder* builder, Value* x, Value* y) const = 0;
  virtual const Type* GetArgType(TypeEnv* env) const = 0;
  virtual const Type* GetReturnType(TypeEnv* env) const = 0;
};

class ArithmeticBinop : public Binop {
 protected:
  ArithmeticBinop(const string& name) : Binop(name) {}
 private:
  virtual const Type* GetArgType(TypeEnv* env) const { return env->GetInt(); }
  virtual const Type* GetReturnType(TypeEnv* env) const { return env->GetInt(); }
};

class Add : public ArithmeticBinop {
 public:
  Add() : ArithmeticBinop("+") {}
  virtual Value* CompilePair(IRBuilder* builder, Value* x, Value* y) const {
    return builder->CreateAdd(x, y);
  }
};
static const Add kAdd;

class Sub : public ArithmeticBinop {
 public:
  Sub() : ArithmeticBinop("-") {}
  virtual Value* CompilePair(IRBuilder* builder, Value* x, Value* y) const {
    return builder->CreateSub(x, y);
  }
};
static const Sub kSub;

class Mul : public ArithmeticBinop {
 public:
  Mul() : ArithmeticBinop("*") {}
  virtual Value* CompilePair(IRBuilder* builder, Value* x, Value* y) const {
    return builder->CreateMul(x, y);
  }
};
static const Mul kMul;

class Div : public ArithmeticBinop {
 public:
  Div() : ArithmeticBinop("/") {}
  virtual Value* CompilePair(IRBuilder* builder, Value* x, Value* y) const {
    return builder->CreateUDiv(x, y);
  }
};
static const Div kDiv;

class RelationalBinop : public Binop {
 private:
  virtual const Type* GetArgType(TypeEnv* env) const { return env->GetInt(); }
  virtual const Type* GetReturnType(TypeEnv* env) const {
    return env->GetBool();
  }
  virtual ICmpInst::Predicate GetPredicate() const = 0;
 protected:
  RelationalBinop(const string& name) : Binop(name) {}
  virtual Value* CompilePair(IRBuilder* builder, Value* x, Value* y) const {
    return builder->CreateICmp(GetPredicate(), x, y);
  }
};

class Equal : public RelationalBinop {
 public:
  Equal() : RelationalBinop("==") {}
 private:
  virtual ICmpInst::Predicate GetPredicate() const {
    return ICmpInst::ICMP_EQ;
  }
};
static const Equal kEqual;

class NotEqual : public RelationalBinop {
 public:
  NotEqual() : RelationalBinop("!=") {}
 private:
  virtual ICmpInst::Predicate GetPredicate() const {
    return ICmpInst::ICMP_NE;
  }
};
static const NotEqual kNotEqual;

class LessThan : public RelationalBinop {
 public:
  LessThan() : RelationalBinop("<") {}
 private:
  virtual ICmpInst::Predicate GetPredicate() const {
    return ICmpInst::ICMP_SLT;
  }
};
static const LessThan kLessThan;

class LessThanOrEqual : public RelationalBinop {
 public:
  LessThanOrEqual() : RelationalBinop("<=") {}
 private:
  virtual ICmpInst::Predicate GetPredicate() const {
    return ICmpInst::ICMP_SLE;
  }
};
static const LessThanOrEqual kLessThanOrEqual;

class GreaterThan : public RelationalBinop {
 public:
  GreaterThan() : RelationalBinop(">") {}
 private:
  virtual ICmpInst::Predicate GetPredicate() const {
    return ICmpInst::ICMP_SGT;
  }
};
static const GreaterThan kGreaterThan;

class GreaterThanOrEqual : public RelationalBinop {
 public:
  GreaterThanOrEqual() : RelationalBinop(">=") {}
 private:
  virtual ICmpInst::Predicate GetPredicate() const {
    return ICmpInst::ICMP_SGE;
  }
};
static const GreaterThanOrEqual kGreaterThanOrEqual;

class ShortCircuitingLogicalOp : public Binop {
 public:
  ShortCircuitingLogicalOp(const string& name) : Binop(name) {}
 private:
  virtual Value* CompilePair(IRBuilder* builder, Value* x, Value* y) const {}
  virtual const Type* GetArgType(TypeEnv* env) const { return env->GetBool(); }
  virtual const Type* GetReturnType(TypeEnv* env) const {
    return env->GetBool();
  }
  virtual void CreateCondBr(IRBuilder* builder, Value* result_of_first,
                            BasicBlock* second, BasicBlock* exit) const = 0;
  virtual Value* Compile(Module* module, IRBuilder* builder, Args* args) const {
    // Because of the short-circuiting, we need some extra basic blocks.  The
    // first arg is compiled into the current basic block, but then we put the
    // second arg in its own basic block (conditionally executed based on the
    // the result of the first operand), and finish off with a basic block to
    // tie the results together.
    Value* first_arg = (*args)[0]->Compile(module, builder);

    BasicBlock* current = builder->GetInsertBlock();
    BasicBlock* eval_second_arg =
        BasicBlock::Create("eval_second_arg", current->getParent());
    BasicBlock* exit =
        BasicBlock::Create("exit", current->getParent());

    CreateCondBr(builder, first_arg, eval_second_arg, exit);
    builder->SetInsertPoint(eval_second_arg);

    Value* second_arg = (*args)[1]->Compile(module, builder);
    builder->CreateBr(exit);
    builder->SetInsertPoint(exit);

    PHINode* final = builder->CreatePHI(eve::types::kLLVMBoolType);
    final->addIncoming(first_arg, current);
    final->addIncoming(second_arg, eval_second_arg);
    return final;
  }
};

class And : public ShortCircuitingLogicalOp {
 public:
  And() : ShortCircuitingLogicalOp("and") {}
 private:
  virtual void CreateCondBr(IRBuilder* builder, Value* result_of_first,
                            BasicBlock* second, BasicBlock* exit) const {
    builder->CreateCondBr(result_of_first, second, exit);
  }
};
static const And kAnd;

class Or : public ShortCircuitingLogicalOp {
 public:
  Or() : ShortCircuitingLogicalOp("or") {}
 private:
  virtual void CreateCondBr(IRBuilder* builder, Value* result_of_first,
                            BasicBlock* second, BasicBlock* exit) const {
    builder->CreateCondBr(result_of_first, exit, second);
  }
};
static const Or kOr;

class Not : public Primitive {
 public:
  Not() : Primitive("not") {}
  virtual const Function* GetType(TypeEnv* env) const {
    FunctionArgs args;
    args.push_back(env->GetBool());
    return env->GetFunction(args, env->GetBool());
  }
  virtual Value* Compile(Module* module, IRBuilder* builder, Args* args) const {
    Value* arg = (*args)[0]->Compile(module, builder);
    return builder->CreateNot(arg);
  }
};
static const Not kNot;

Funcall::Funcall(const Location& location, const char* op, Args* args) 
    : Expr(location), op_(op), args_(args) {}

Funcall::~Funcall() {
  for(Args::iterator iter = args_->begin(); iter != args_->end(); ++iter) {
    delete *iter;
  }
  delete args_;
}

const Type* Funcall::TypeCheck(TypeEnv* env) const {
  Primitive* maybe_primitive = primitives[op_];
  if (maybe_primitive) {
    const Function* primitive_type = maybe_primitive->GetType(env);
    const FunctionArgs& arg_types = primitive_type->GetArgTypes();
    if (arg_types.size() != args_->size()) {
      // TODO: include the expected and found numbers.
      env->AddError(GetLocation(), "Wrong number of arguments.");
      return NULL;
    }
    
    bool seenError = false;
    FunctionArgs::const_iterator type = arg_types.begin();
    for (Args::const_iterator expr = args_->begin(),
         end = args_->end(); expr != end; ++type, ++expr) {
      if (*type != (*expr)->TypeCheck(env)) {
        env->AddError(GetLocation(), "Type mismatch.");
        seenError = true;
      }
    }
    return seenError ? NULL : primitive_type->GetReturnType();
  } else {
    std::cout << "No primitive for " << op_ << std::endl;
    assert(false);
  }
}

Value* Funcall::Compile(Module* module, IRBuilder* builder) const {
  Primitive* maybePrimitive = primitives[op_];
  if (maybePrimitive) {
    return maybePrimitive->Compile(module, builder, args_);
  } else {
    std::cout << "No primitive for " << op_ << std::endl;
    assert(false);
  }
}

string Funcall::PPrint() const {
  std::stringstream stream;
  stream << '(' << op_;
  for (Args::iterator iter = args_->begin(); iter != args_->end(); ++iter) {
    stream << ' ' << (*iter)->PPrint();
  }
  return stream.str();
}

} // namespace expr
} // namespace eve
