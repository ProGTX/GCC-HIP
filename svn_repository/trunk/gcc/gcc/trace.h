#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>

/* Obtain a backtrace and print it to stdout. */
#define PRINT_TRACE()								\
{													\
	void *array[20];								\
	size_t size;									\
	char **strings;									\
	size_t i;										\
	size = backtrace (array, 20);					\
	strings = backtrace_symbols (array, size);		\
	printf("Obtained %zd stack frames.\n", size);	\
	for (i = 0; i < size; i++)						\
		printf ("%s\n", strings[i]);				\
	free (strings);									\
}