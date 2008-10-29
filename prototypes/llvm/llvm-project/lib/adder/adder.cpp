#include <stdlib.h>
#include <adder.h>

Adder::Adder() : sum(0) {}

void
Adder::add_value(const char* val)
{
    sum += atoi(val);
}

int
Adder::get_value() const
{
    return sum;
}
