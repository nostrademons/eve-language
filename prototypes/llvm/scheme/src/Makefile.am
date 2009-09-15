bin_PROGRAMS = calculator
CC = $(CXX)
SUBDIRS = expr
AM_DEFAULT_SOURCE_EXT = .cpp
calculator_SOURCES = location.h location.cpp parser.h lexer.l parser.y main.c
calculator_LDADD = expr/libschemeexpr.a
calculator_CPPFLAGS = $(LLVM_CPPFLAGS) $(AM_CPPFLAGS)
calculator_LDFLAGS = $(LLVM_LDFLAGS) $(AM_LDFLAGS)

parser.c parser.tab.h: parser.y
	@YACC@ -d -p eve_yy $<