#ifndef LITERAL_H
#define LITERAL_H

#include "expr.h"

class Literal : public Expr {
	  int _value;
  public:
    Literal(int value);
    ~Literal();
    virtual int eval();
    virtual std::string pprint();
};

#endif