#include <llvm/BasicBlock.h>
#include <llvm/CallingConv.h>
#include <llvm/Constant.h>
#include <llvm/Constants.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/Instruction.h>
#include <llvm/Instructions.h>
#include <llvm/Module.h>
#include <llvm/ModuleProvider.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <stdio.h>

using namespace llvm;

Module*
makeLLVMModule() {
	Module* mod = new Module("test");
	Constant* c = mod->getOrInsertFunction("factorial", IntegerType::get(32),
		IntegerType::get(32), IntegerType::get(32), NULL);
	Function* factorial = cast<Function>(c);
	factorial->setCallingConv(CallingConv::C);
	
	BasicBlock* entry = BasicBlock::Create("entry", factorial);
	BasicBlock* test = BasicBlock::Create("test", factorial);
	BasicBlock* loop = BasicBlock::Create("loop", factorial);
	BasicBlock* ret = BasicBlock::Create("ret", factorial);

	Function::arg_iterator args = factorial->arg_begin();
	Argument* x = args++;
	Value* one = ConstantInt::get(IntegerType::get(32), 1);
	BranchInst::Create(test, entry);
	
	PHINode* accum = PHINode::Create(IntegerType::get(32), "accum", test);
	PHINode* current = PHINode::Create(IntegerType::get(32), "current", test);
	accum->addIncoming(one, entry);
	current->addIncoming(x, entry);
	Value* testSucceeded = new ICmpInst(ICmpInst::ICMP_SGT, current, one, "cond", test);
	BranchInst::Create(loop, ret, testSucceeded, test);
	
	Value* nextAccum = BinaryOperator::CreateMul(accum, current, "nextAccum", loop);
	Value* nextCurrent = BinaryOperator::CreateSub(current, one, "nextCurrent", loop);
	accum->addIncoming(nextAccum, loop);
	current->addIncoming(nextCurrent, loop);
	BranchInst::Create(test, loop);
	
	ReturnInst::Create(accum, ret);

	return mod;
}

int
main (int argc, char const *argv[])
{
	Module* mod = makeLLVMModule();
	verifyModule(*mod, PrintMessageAction);

	ExecutionEngine* jit = ExecutionEngine::create(mod);
	Function* factorial = mod->getFunction("factorial");
	int (*native_factorial)(int) = (int (*)(int)) jit->getPointerToFunction(factorial);
	int arg1 = argc > 1 ? atoi(argv[1]) : 1;
	printf("Factorial(%d) = %d.\n", arg1, native_factorial(arg1));
	return 0;
}