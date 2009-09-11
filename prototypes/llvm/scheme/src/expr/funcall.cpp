#include "funcall.h"

#include <assert.h>
#include <iostream>
#include <map>
#include <sstream>
#include <string>

#include <llvm/Support/IRBuilder.h>

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
  virtual int eval(Args* args) = 0;
  virtual Value* compile(Module* module, IRBuilder* builder, Args* args) = 0;
  virtual const string& pprint() { return name_; }
};

class Binop : public Primitive {
private:
  virtual int evalPair(int x, int y) = 0;
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) = 0;
public:
  Binop(const string& name) : Primitive(name) {}
  virtual int eval(Args* args) {
    int current = (*args)[0]->eval();
    for(Args::iterator iter = ++args->begin(); iter != args->end(); ++iter) {
      current = evalPair(current, (*iter)->eval());
    }
    return current;
  }
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
  virtual int evalPair(int x, int y) { return x + y; }
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) {
    return builder->CreateAdd(x, y);
  }
};
static const Add kAdd;

class Sub : public Binop {
public:
  Sub() : Binop("-") {}
  virtual int evalPair(int x, int y) { return x - y; }
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) {
    return builder->CreateSub(x, y);
  }
};
static const Sub kSub;

class Mul : public Binop {
public:
  Mul() : Binop("*") {}
  virtual int evalPair(int x, int y) { return x * y; }
  virtual Value* compilePair(IRBuilder* builder, Value* x, Value* y) {
    return builder->CreateMul(x, y);
  }
};
static const Mul kMul;

class Div : public Binop {
public:
  Div() : Binop("/") {}
  virtual int evalPair(int x, int y) { return x / y; }
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

int Funcall::eval() {
  Primitive* maybePrimitive = primitives[op_];
  if (maybePrimitive) {
    return maybePrimitive->eval(args_);
  } else {
    // TODO: user-defined functions.
    assert(false);
  }
}

Value* Funcall::compile(Module* module, IRBuilder* builder) {
  Primitive* maybePrimitive = primitives[op_];
  if (maybePrimitive) {
    return maybePrimitive->compile(module, builder, args_);
  } else {
    std::cout << "No primitive for " << op_ << std::endl;
    assert(false);
  }
}

string Funcall::pprint() {
  std::stringstream stream;
  stream << '(' << op_;
  for(Args::iterator iter = args_->begin(); iter != args_->end(); ++iter) {
    stream << ' ' << (*iter)->pprint();
  }
  return stream.str();
}
