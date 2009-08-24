bin_PROGRAMS = calculator
CC = $(CXX)
calculator_SOURCES = sexp.h parser.h lexer.l parser.y main.c

parser.c parser.tab.h: parser.y
	@YACC@ -d $<