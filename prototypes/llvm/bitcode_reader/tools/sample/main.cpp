#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

#include "ModuleReader.h"

int
main (int argc, char ** argv)
{
    if(argc <= 1) {
        printf("Usage: bitcode <filename.bc> <arg1> <arg2> ...");
        exit(1);
    }
    ModuleReader reader(argv[1]);
    int (*main_fn)(int, char**) = (int (*)(int, char**)) reader.get_function("main");
    if(!main_fn) {
        printf("Couldn't find main function");
        exit(1);
    }

    char** new_argv = new char*[argc - 1];
    new_argv[0] = argv[0];
    for(int i = 2; i < argc; ++i) {
        new_argv[i - 1] = argv[i];
    }
    int exit_status = main_fn(argc - 1, new_argv);
    delete[] new_argv;
    exit (exit_status);
}

