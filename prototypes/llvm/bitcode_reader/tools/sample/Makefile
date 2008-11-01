##===- projects/sample/tools/sample/Makefile ---------------*- Makefile -*-===##

#
# Indicate where we are relative to the top of the source tree.
#
LEVEL=../..

#
# Give the name of the tool.
#
TOOLNAME=bitcode

#
# List libraries that we'll need
# We use LIBS because sample is a dynamic library.
#
USEDLIBS = bitcode_reader.a
LLVMLIBS = LLVMX86 LLVMSelectionDAG.a LLVMBitReader.a LLVMJIT LLVMExecutionEngine LLVMCodeGen.a LLVMScalarOpts.a LLVMTransformUtils.a LLVMAnalysis.a LLVMipa.a LLVMTarget.a LLVMCore.a LLVMSupport.a LLVMSystem.a

#
# Include Makefile.common so we know what to do.
#
include $(LEVEL)/Makefile.common

