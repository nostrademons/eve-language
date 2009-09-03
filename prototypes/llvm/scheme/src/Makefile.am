bin_PROGRAMS = calculator
CC = $(CXX)
SUBDIRS = expr
AM_DEFAULT_SOURCE_EXT = .cpp
calculator_SOURCES = sexp.h parser.h lexer.l parser.y main.c
calculator_LDADD = expr/libschemeexpr.a

parser.c parser.tab.h: parser.y
	@YACC@ -d $<