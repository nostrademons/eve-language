bin_PROGRAMS = calculator
CC = $(CXX)
SUBDIRS = expr types
AM_DEFAULT_SOURCE_EXT = .cpp
calculator_SOURCES = location.h location.cpp parser.h repl.h repl.cpp lexer.l \
    parser.y main.c
calculator_LDADD = expr/libschemeexpr.a types/libschemetypes.a
calculator_CPPFLAGS = $(BOOST_CPPFLAGS) $(LLVM_CPPFLAGS) $(AM_CPPFLAGS)
calculator_LDFLAGS = $(BOOST_LDFLAGS) $(LLVM_LDFLAGS) $(AM_LDFLAGS)

parser.c parser.tab.h: parser.y
	@YACC@ -d -p eve_yy $<
