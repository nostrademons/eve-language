#include "funcall.h"

#include <assert.h>
#include <iostream>
#include <map>
#include <sstream>
#include <string>

#include <llvm/Support/IRBuilder.h>

#include "../types/bool.h"
#include "../types/int.h"
#include "../types/type_env.h"

namespace eve {
namespace types {
  class Type;
}

namespace expr {

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
  virtual Value* compile(Module* module, IRBuilder* builder, Args* args) = 0;
  virtual const string& pprint() { return name_; }
};

class Binop : public Primitive {
private:
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) = 0;
public:
  Binop(const string& name) : Primitive(name) {}
  virtual Value* compile(Module* module, IRBuilder* builder, Args* args) {
    Value* current = (*args)[0]->compile(module, builder);
    for (Args::iterator i = ++args->begin(), e = args->end(); i != e; ++i) {
      Value* next = (*i)->compile(module, builder);
      current = compilePair(builder, current, next);
    }
    return current;
  }
};

class Add : public Binop {
public:
  Add() : Binop("+") {}
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) {
    return builder->CreateAdd(x, y);
  }
};
static const Add kAdd;

class Sub : public Binop {
public:
  Sub() : Binop("-") {}
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) {
    return builder->CreateSub(x, y);
  }
};
static const Sub kSub;

class Mul : public Binop {
public:
  Mul() : Binop("*") {}
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) {
    return builder->CreateMul(x, y);
  }
};
static const Mul kMul;

class Div : public Binop {
public:
  Div() : Binop("/") {}
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) {
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
  // TODO: Should depend on the particular primitive invoked.
  return env->GetInt();
}

Value* Funcall::compile(Module* module, IRBuilder* builder) const {
  Primitive* maybePrimitive = primitives[op_];
  if (maybePrimitive) {
    return maybePrimitive->compile(module, builder, args_);
  } else {
    std::cout << "No primitive for " << op_ << std::endl;
    assert(false);
  }
}

string Funcall::pprint() const {
  std::stringstream stream;
  stream << '(' << op_;
  for(Args::iterator iter = args_->begin(); iter != args_->end(); ++iter) {
    stream << ' ' << (*iter)->pprint();
  }
  return stream.str();
}

} // namespace expr
} // namespace eve