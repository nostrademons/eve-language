AC_INIT([scheme], [0.1], [jonathan.d.tang@gmail.com])
AC_CONFIG_AUX_DIR([autoconf])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CXX
AC_PROG_LEX
AC_PROG_YACC
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([autoconf])
AC_CONFIG_FILES([
	Makefile
	src/Makefile
])
AX_LLVM([core jit native])
AC_OUTPUT