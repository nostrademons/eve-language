#include "llvm/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"

class ModuleReader
{
private:
    llvm::Module * module;
    llvm::ExecutionEngine * jit;

    ModuleReader(const ModuleReader& original);

public:
    explicit ModuleReader(const char * module_name);
    ~ModuleReader();

    void* get_function(const char * name) const;
};
