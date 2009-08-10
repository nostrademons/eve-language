#
# Configure a Makefile without clobbering it if it exists and is not out of
# date.  This macro is unique to LLVM.
#
AC_DEFUN([AC_CONFIG_MAKEFILE],
[AC_CONFIG_COMMANDS($1,
  [autoconf/mkinstalldirs `dirname $1`
   ${SHELL} autoconf/install-sh -c ${srcdir}/$1 $1])
])
