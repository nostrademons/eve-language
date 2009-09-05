#include "funcall.h"

#include <sstream>

#include <llvm/Support/IRBuilder.h>

using namespace llvm;

Funcall::Funcall(const Location& location, OpType op, Args* args) 
    : Expr(location), _op(op), _args(args) {}
Funcall::~Funcall() {
  for(Args::iterator iter = _args->begin(); iter != _args->end(); ++iter) {
    delete *iter;
  }
  delete _args;
}

int Funcall::eval() {
  int initial = (*_args)[0]->eval();
  switch(_op) {
    case OpPlus:
    for(Args::iterator iter = ++_args->begin(); iter != _args->end(); ++iter) {
      initial += (*iter)->eval();
    }
    return initial;
    case OpMinus:
    for(Args::iterator iter = ++_args->begin(); iter != _args->end(); ++iter) {
      initial -= (*iter)->eval();
    }
    return initial;
    case OpTimes:
    for(Args::iterator iter = ++_args->begin(); iter != _args->end(); ++iter) {
      initial *= (*iter)->eval();
    }
    return initial;
    case OpDivide:
    for(Args::iterator iter = ++_args->begin(); iter != _args->end(); ++iter) {
      initial /= (*iter)->eval();
    }
    return initial;
	}
}

Value* Funcall::compile(Module* module, IRBuilder* builder) {
  Value* current = (*_args)[0]->compile(module, builder);
  for (Args::iterator i = ++_args->begin(), e = _args->end(); i != e; ++i) {
    Value* next = (*i)->compile(module, builder);
    switch(_op) {
      case OpPlus:
      current = builder->CreateAdd(current, next);
      break;
      case OpMinus:
      current = builder->CreateSub(current, next);
      break;
      case OpTimes:
      current = builder->CreateMul(current, next);
      break;
      case OpDivide:
      current = builder->CreateUDiv(current, next);
      break;
    }
  }
  return current;
}

std::string Funcall::pprint() {
  std::stringstream stream;
  stream << '(';
  switch(_op) {
    case OpPlus: stream << '+'; break;
    case OpMinus: stream << '-'; break;
    case OpTimes: stream << '*'; break;
    case OpDivide: stream << '/'; break;
  }
  for(Args::iterator iter = _args->begin(); iter != _args->end(); ++iter) {
    stream << ' ' << (*iter)->pprint();
  }
  return stream.str();
}
