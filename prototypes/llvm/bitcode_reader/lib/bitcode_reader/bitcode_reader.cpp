#include "ModuleReader.h"

#include "llvm/Module.h"
#include "llvm/Function.h"
#include "llvm/ModuleProvider.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"

#include <stdlib.h>
#include <assert.h>
#include <string>

void die(const char* module_name, std::string error)
{
    printf("Error in %s: %s\n", module_name, error.c_str());
    exit(1);
}

ModuleReader::ModuleReader(const char* module_name)
{
    std::string error;

    if(llvm::MemoryBuffer* buffer = llvm::MemoryBuffer::getFile(module_name, &error)) {
        module = llvm::ParseBitcodeFile(buffer, &error);
        if(!module) {
            die(module_name, error);
        } else {
            delete buffer;
            llvm::ExistingModuleProvider* mp = new llvm::ExistingModuleProvider(module);
            jit = llvm::ExecutionEngine::create(mp, false, &error);
            if(!jit) {
                die(module_name, error);
            }
        }
    } else {
        die(module_name, error);
    }
}

ModuleReader::~ModuleReader()
{
    delete jit;
}

void*
ModuleReader::get_function(const char* function_name) const
{
    assert(jit);
    llvm::Function* fn = module->getFunction(function_name);
    if(!fn) {
        return NULL;
    }
    return jit->getPointerToFunction(fn);
}
