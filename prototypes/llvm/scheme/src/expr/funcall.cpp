#include "funcall.h"

#include <assert.h>
#include <iostream>
#include <map>
#include <sstream>
#include <string>

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
using llvm::IRBuilder;
using llvm::Module;
using llvm::Value;
using std::string;

class Primitive;
static std::map<string, Primitive*> primitives;

class Primitive {
private:
  string name_;
protected:  
  Primitive(const string& name) : name_(name) {
    primitives[name] = this;
  }
public:
  virtual const Function* GetType(TypeEnv* env) const = 0;
  virtual Value* Compile(Module* module, IRBuilder* builder, Args* args) const = 0;
  virtual const string& PPrint() const { return name_; }
};

class ArithmeticBinop : public Primitive {
private:
  virtual Value* CompilePair(IRBuilder* builder, Value* x, Value* y) const = 0;
public:
  ArithmeticBinop(const string& name) : Primitive(name) {}
  virtual const Function* GetType(TypeEnv* env) const {
    FunctionArgs args;
    args.push_back(env->GetInt());
    args.push_back(env->GetInt());
    return env->GetFunction(args, env->GetInt());
  }
  virtual Value* Compile(Module* module, IRBuilder* builder, Args* args) const {
    Value* first_arg = (*args)[0]->Compile(module, builder);
    Value* second_arg = (*args)[1]->Compile(module, builder);
    return CompilePair(builder, first_arg, second_arg);
  }
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