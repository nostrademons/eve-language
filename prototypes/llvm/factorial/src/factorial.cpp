#include <llvm/BasicBlock.h>
#include <llvm/CallingConv.h>
#include <llvm/Constant.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/Instruction.h>
#include <llvm/Module.h>
#include <llvm/ModuleProvider.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Support/IRBuilder.h>
#include <stdio.h>

using namespace llvm;

Module*
makeLLVMModule() {
	Module* mod = new Module("test");
	Constant* c = mod->getOrInsertFunction("adder", IntegerType::get(32),
		IntegerType::get(32), IntegerType::get(32), NULL);
	Function* adder = cast<Function>(c);
	adder->setCallingConv(CallingConv::C);
	
	BasicBlock* block = BasicBlock::Create("entry", adder);
	IRBuilder builder(block);
	Function::arg_iterator args = adder->arg_begin();
	Argument* x = args++;
	Argument* y = args++;
	builder.CreateRet(builder.CreateBinOp(Instruction::Add, x, y, "tmp"));
	return mod;
}

int
main (int argc, char const *argv[])
{
	Module* mod = makeLLVMModule();
	verifyModule(*mod, PrintMessageAction);

	ExecutionEngine* jit = ExecutionEngine::create(mod);
	Function* adder = mod->getFunction("adder");
	int (*native_adder)(int, int) = (int (*)(int, int)) jit->getPointerToFunction(adder);
	printf("Hello, %d!\n", native_adder(2, 3));
	return 0;
}