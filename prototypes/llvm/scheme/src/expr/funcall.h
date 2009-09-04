#ifndef FUNCALL_H
#define FUNCALL_H

#include <memory>
#include <string>
#include <vector>

#include "expr.h"

namespace llvm {
  class IRBuilder;
  class Module;
  class Value;
}

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
    virtual std::auto_ptr<llvm::Value>
        compile(llvm::Module& module, llvm::IRBuilder& builder);
    virtual std::string pprint();
};

#endif