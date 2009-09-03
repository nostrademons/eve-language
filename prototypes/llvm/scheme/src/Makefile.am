bin_PROGRAMS = calculator
CC = $(CXX)
AM_DEFAULT_SOURCE_EXT = .cpp
calculator_SOURCES = sexp.h parser.h lexer.l parser.y main.c literal.cpp funcall.cpp

parser.c parser.tab.h: parser.y
	@YACC@ -d $<