#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "utils.h"
#include "str.h"


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
	if (arr->type_t == String){
		string **data = (string **)arr->data;
		for (int i = 0; i < arr->length; i++){
			cnet_free(data[i]);
		}
	}
	free(arr->data);
	free(arr);
}

cnet_array *cnet_init_array(int sizei_t, int type_t, int len, int arr_lit_len, ...)
{
	cnet_array *new_arr = (cnet_array *)mem_alloc(sizeof(cnet_array));

	new_arr->cnet_free  = free_cnet_array;
	new_arr->data	    = (char *)mem_alloc(len * sizei_t);
	new_arr->length	    = len;
	new_arr->i_t	    = sizei_t;
	new_arr->type_t		= type_t;

	printf("len:%d, arr_lit_len:%d\n", len, arr_lit_len);
	va_list arr_list;

	va_start(arr_list, arr_lit_len);
	int c;
	int n;
	unsigned long ptr;
	double d;
	

	for (int i = 0; i<(arr_lit_len*sizei_t); i+=sizei_t){
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
		else if (sizei_t == 8 && type_t == Float){
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

	if (type_t != String)
		goto out;

	for (int i = arr_lit_len*sizei_t; i<(len*sizei_t); i+=sizei_t){
		string *new_str = cnet_empty_str();
		memcpy(new_arr->data+i, &new_str, sizei_t);
	}

out:
	return new_arr;

}

void *cnet_index_arr(void *ptr, int index)
{
	char s[100];

	cnet_array *arr = (cnet_array *)ptr;

	sprintf(s, "Index %d is out of range for array", index);

	// printf("Arr_len:%d\n", arr->length);
	if (index > arr->length)
		die(s);

	return (void *)(((char *)arr->data)+arr_offset(index, arr->i_t));

}

int cnet_arr_length(void *ptr)
{
	cnet_array *arr = (cnet_array *)ptr;

	return arr->length;
}

cnet_array *parse_main_args(int argc, char **argv)
{
	cnet_array *args = cnet_init_array(8, String, argc, 0);
	string **data = (string **)args->data;
	for(int i = 0 ; i < argc; i++){
		string *new_str = cnet_new_str_nolen(argv[i]);
		cnet_strcpy(data[i], new_str);
		cnet_free(new_str);
	}

	return args;
}

int main(int argc, char **argv)
{
	printf("argc:%d argv[0]: %s\n", argc, argv);
	cnet_array *string_arr = parse_main_args(argc-1, &argv[1]);
	int ret = user_main(string_arr);
	cnet_free(string_arr);
	return ret;
}