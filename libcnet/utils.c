#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
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

static int inline arr_offset(int i, int sizei_t)
{
	return i*sizei_t;
}

static void free_cnet_array(void *ptr)
{
	cnet_array *arr = (cnet_array *)ptr;
	free(arr->data);
	free(arr);
}

cnet_array *cnet_init_array(int len, int sizei_t, ...)
{
	cnet_array *new_arr = (cnet_array *)mem_alloc(sizeof(cnet_array));

	new_arr->cnet_free  = free_cnet_array;
	new_arr->data	    = (char *)mem_alloc(len * sizei_t);
	new_arr->length	    = len;
	new_arr->i_t	    = sizei_t;

	va_list arr_list;

	va_start(arr_list, len);
	int c;
	int n;
	char *ptr;

	for (int i = 0; i<(len*sizei_t); i+=sizei_t){
		switch(sizei_t) {
			case 1:
				c = va_arg(arr_list, char);
				memcpy(new_arr->data+i, &c, sizei_t);
			case 4:
				n = va_arg(arr_list, int);
				memcpy(new_arr->data+i, &n, sizei_t);
			case 8:
				ptr = va_arg(arr_list, char *);
				memcpy(new_arr->data+i, &ptr, sizei_t);

		}
	}

	va_end(arr_list);

	return new_arr;

} 

void *cnet_index_arr(void *ptr, int index)
{
	cnet_array *arr = (cnet_array *)ptr;

	return (void *)((char *)arr->data)+arr_offset(index, arr->i_t);
}

int cnet_arr_length(void *ptr)
{
	cnet_array *arr = (cnet_array *)ptr;
	
	return arr->length;
}
