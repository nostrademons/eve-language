#include "funcall.h"

#include <sstream>

Funcall::Funcall(OpType op, Args* args) : _op(op), _args(args) {}
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
