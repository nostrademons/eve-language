BUILT_SOURCES = parser.h
AM_YFLAGS = -d

lib_LTLIBRARIES = libscheme.la
libscheme_la_SOURCES = lexer.l parser.y sexp.cpp
libscheme_la_CPPFLAGS = $(LLVM_CPPFLAGS) $(AM_CPPFLAGS)
libscheme_la_LDFLAGS = $(LLVM_LDFLAGS) $(AM_LDFLAGS)
include_headers = scheme.h

bin_PROGRAMS = pprint
pprint_SOURCES = pprint.cpp
pprint_LDADD = libscheme.la