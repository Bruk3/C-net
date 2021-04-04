#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "string.h"


static void die(const char *message)
{
	perror(message);
	exit(1);
}

static void *mem_alloc(int length)
{
	void *mem = malloc(length);

	if (!mem)
		die("Could not allocate memory");

	return mem;
}

static string *create_str()
{
	string *new_str = (string *) mem_alloc(sizeof(string));

	new_str->length 	= 0;
	new_str->data		= NULL;

	return new_str;
}

/* Constructors */ 
string *new_str()
{
	return create_str();
}

string *new_str_with_data(char *data)
{
	string *new_str =  create_str();
	new_str->length = strlen(data);
	new_str->data	= (char *)mem_alloc(new_str->length+1);
	strncpy(new_str->data, data, new_str->length+1);

	return new_str;
}

/* Destructor */
void free_str(string *str)
{
	if (str->length)
		free(str->data);
	free(str);
}

/* Copy assignment */
void copy_str(string *src, string *dst)
{
	if (dst->length < src->length){
		free(dst->data);
		dst->data = (char *) mem_alloc(src->length+1);
	}

	strncpy(dst->data, src->data, dst->length+1);
	dst->length = src->length;
}

static void __concat_str(char *s, string *s1, string *s2)
{
	s = (char *) mem_alloc(s1->length+s2->length+1);
	strncpy(s, s1->data, s1->length);
	strncpy(s+s1->length, s2->data, s2->length+1);

}
/* Operator + */
string *concat_str(string *s1, string *s2)
{
	string *new_str = create_str();
	new_str->length = s1->length+s2->length;
	__concat_str(new_str->data, s1, s2);

	return new_str;
}

/* Operator += */ 
void merge_str(string *s1, string *s2)
{
	char *temp_data = (char *) mem_alloc(s2->length+s1->length+1);	
	__concat_str(temp_data, s1, s2);
	if (s1->length)
		free(s1->data);
	
	s1->length += s2->length;
	s1->data    = temp_data;
}

/* Operator ==, >, < */ 
int cmp_str(string *s1, string *s2)
{
	return strcmp(s1->data, s2->data);
}

/* Operator [] */ 
char char_at(string *str, int index)
{
	if (index >= str->length)
		return '\0';
 
	return str->data[index];
}

/* Other helper function */ 
int str_length(string *s1)
{
	return s1->length;
}