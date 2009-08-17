#include <stdio.h>

int calculate(char* input);

int main (int argc, char const *argv[])
{	
	if(argc <= 1) {
		printf("usage: calculator \"2 + 2\"");
		return -1;
	}
	printf("'%s' is %d", argv[1], calculate(argv[1]));
	return 0;
}