#ifndef SEXP_H
#define SEXP_H

typedef struct Cons Cons;
typedef struct SExp SExp;

struct Cons {
	SExp* car;
	SExp* cdr;
};

struct SExp {
	enum SExpType {
		BOOL_TYPE, INT_TYPE, DOUBLE_TYPE, STRING_TYPE, SYMBOL_TYPE, CONS_TYPE
	} type;
	union {
		bool b;
		int i;
		double d;
		const char* s;
		Cons* c;
	} value;
};

#endif