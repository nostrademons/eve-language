#ifndef FUNCALL_H
#define FUNCALL_H

#include <string>
#include <vector>

#include <llvm/Value.h>

#include "expr.h"

typedef std::vector<Expr*> Args;

class Funcall : public Expr {
  public:
    typedef enum { OpPlus, OpMinus, OpTimes, OpDivide } OpType;
  private:
	  Args* _args;
	  OpType _op;
  public:
    Funcall(OpType op, Args* args);
    virtual ~Funcall();
    virtual int eval();
    virtual llvm::Value* compile();
    virtual std::string pprint();
};

#endif