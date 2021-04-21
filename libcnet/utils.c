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

cnet_array *cnet_init_array(int sizei_t, int floating, int len, ...)
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
	unsigned long ptr;
	double d;
	

	for (int i = 0; i<(len*sizei_t); i+=sizei_t){
		if (sizei_t == 1){
			c = va_arg(arr_list, int);
			memcpy(new_arr->data+i, &c, sizei_t);
			// printf("arr[%c] = %c\n", i/sizei_t, *((char *)new_arr->data+i) );
		}
		else if(sizei_t == 4){
			n = va_arg(arr_list, int);
			memcpy(new_arr->data+i, &n, sizei_t);
			// printf("arr[%d] = %d\n", i/sizei_t, *((int *)new_arr->data+(i/sizei_t)) );
		}
		else if (sizei_t == 8 && floating){
			d = va_arg(arr_list, double);
			memcpy(new_arr->data+i, &d, sizei_t);
			// printf("arr[%d] = %f\n", i/sizei_t, *((double *)new_arr->data+(i/sizei_t)) );
		}
		else {
			ptr = va_arg(arr_list, long);
			memcpy(new_arr->data+i, &ptr, sizei_t);
			// printf("arr[%d] = %lu\n", i/sizei_t, *((unsigned long *)new_arr->data+(i/sizei_t)) );
		}

	}

	va_end(arr_list);

	return new_arr;

} 

void *cnet_index_arr(void *ptr, int index)
{
	cnet_array *arr = (cnet_array *)ptr;

	return (void *)(((char *)arr->data)+arr_offset(index, arr->i_t));
}

int cnet_arr_length(void *ptr)
{
	cnet_array *arr = (cnet_array *)ptr;
	
	return arr->length;
}
