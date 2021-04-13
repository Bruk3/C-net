#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "utils.h"


void die(const char *message)
{
	perror(message);
	exit(1);
}

void *mem_alloc(int size)
{
	void *mem = malloc(size);

	if (!mem)
		die("Could not allocate memory");

	return mem;
}

/* free yourself */ 
void cnet_free(void *s)
{
	cnet_io *fs = (cnet_io *)s;
	if(s)
		fs->cnet_free(s);
}
