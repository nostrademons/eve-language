#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

#include <adder.h>

int
main (int argc, char ** argv)
{
    Adder adder;
    for(int i = 1; i < argc; ++i) {
        adder.add_value(argv[i]);
    }
    printf ("Sum: %d\n", adder.get_value());
    exit (0);
}

