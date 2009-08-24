#include <stdio.h>
#include <sstream>
#include "parser.h"

int main (int argc, char const *argv[])
{	
	if(argc <= 1) {
		printf("usage: calculator \"2 + 2\"");
		return -1;
	}
	std::stringstream input(argv[1]);
	Parser parser;
	printf("'%s' is %d.\n", argv[1], parser.parse(input));
	return 0;
}