#!/bin/sh
die () {
	echo "$@" 1>&2
	exit 1
}
test -d autoconf && test -f autoconf/configure.ac && cd autoconf
test -f configure.ac || die "Can't find 'autoconf' dir; please cd into it first"
autoconf --version | egrep '2\.6[0-9]' > /dev/null
if test $? -ne 0 ; then
  die "Your autoconf was not detected as being 2.5x"
fi

llvm_m4=/home/jonathan/software/llvm-2.3/autoconf/m4
llvm_src_root=/home/jonathan/software/llvm-2.3/
llvm_obj_root=/home/jonathan/software/llvm-2.3/
# Patch the LLVM_ROOT in configure.ac, if it needs it
cp configure.ac configure.bak
sed -e "s#^LLVM_SRC_ROOT=.*#LLVM_SRC_ROOT=\"$llvm_src_root\"#" \
    -e "s#^LLVM_OBJ_ROOT=.*#LLVM_OBJ_ROOT=\"$llvm_obj_root\"#" configure.bak > configure.ac
echo "Regenerating aclocal.m4 with aclocal"
rm -f aclocal.m4
aclocal -I $llvm_m4 -I "$llvm_m4/.." || die "aclocal failed"
echo "Regenerating configure with autoconf 2.5x"
autoconf --warnings=all -o ../configure configure.ac || die "autoconf failed"
cd ..
exit 0
