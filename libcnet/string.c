#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "utils.h"


/* Destructor */
static void cnet_free_str(void *str)
{
	if (!str)
		return;

	string *s = (string *)str;
	if (s->length)
		free(s->data);

	free(s);
}

static string *create_str()
{
	string *new_str = (string *) mem_alloc(sizeof(string));

	new_str->cnet_free  = cnet_free_str;
	new_str->length 	= 0;
	new_str->data		= NULL;

	return new_str;
}

static string *clone_str(string *s)
{
	string *new_str = (string *) mem_alloc(sizeof(string));

	new_str->cnet_free  = cnet_free_str;
	new_str->length 	= s->length;
	new_str->data		= s->length == 0 ? NULL :
							(char *) mem_alloc(s->length);

	return new_str;
}
static void __concat_str(char **s, string *s1, string *s2)
{
	*s = (char *) mem_alloc(s1->length+s2->length);
	if(s1->data)
		memcpy(*s, s1->data, s1->length);
	if(s2->data)
		memcpy((*s)+s1->length, s2->data, s2->length);
}

/* Constructors */
string *cnet_empty_str()
{
	return create_str();
}

string *cnet_new_str(char *data, int length)
{
	if (!length)
		return cnet_empty_str();

	string *new_str 	=  create_str();

	new_str->length 	= length;
	new_str->data		= (char *)mem_alloc(length);
	memcpy(new_str->data, data, length);

	return new_str;
}

/* char *data must be null terminated */
string *cnet_new_str_nolen(char* data)
{
	return cnet_new_str(data, strlen(data));
}

/*(deep copy) eg.
 * string s1 = "Hi";
 * string s2 = "Hell0";
 * s3 = (s1 = s2);
 */
string *cnet_strcpy(string *dst, string *src)
{
	if (!dst || !src)
		die("Error: Null Pointer\n");

	if (dst->data)
		free(dst->data);

	dst->length = src->length;
	dst->data = (char *) mem_alloc(src->length);

	memcpy(dst->data, src->data, src->length);

	return dst;
}

/* operator eg.
 * string *s = "Hell0";
 * string *s1 = s; */
string *cnet_strassign(string *s)
{
	string *str = cnet_empty_str();

	cnet_strcpy(str, s);

	return str;
}

/* Operator + */
string *cnet_strcat(string *s1, string *s2)
{
	if (!s1 || !s2)
		die("Error: Null Pointer\n");

	char *temp_data;
	__concat_str(&temp_data, s1, s2);

	string *new_str = create_str();

	new_str->length = s1->length+s2->length;
	new_str->data	= temp_data;

	return new_str;
}

/* Operator += */
void cnet_strmerge(string *s1, string *s2)
{
	if (!s1 || !s2)
		die("Error: Null Pointer\n");

	char *temp_data;

	__concat_str(&temp_data, s1, s2);

	if (s1->data)
		free(s1->data);

	s1->length += s2->length;
	s1->data    = temp_data;
}

/* Operator * */
string *cnet_strmult(string *s, int mult)
{

	if (!s || mult < 0)
		die("Error: Invalid argument");

	string *new_str = cnet_empty_str();

	if (s->length == 0)
		return new_str;

	new_str->length = s->length * mult;
	new_str->data	= (char *) mem_alloc(new_str->length);

	for (int i = 0; i < mult; i++)
		memcpy(&new_str->data[i*s->length], s->data, s->length);

	return new_str;
}

/* Operator == */
int cnet_strcmp(string *s1, string *s2)
{
	if (!s1 && !s2)
		return 0;

	if (!s1 || !s2)
		return -1;

	if (s1->length != s2->length)
		return -1;

	for(int i = 0; i<s1->length;i++)
		if (s1->data[i] != s2->data[i])
			return -1;

	return 0;
}

/* Operator [] */
char cnet_char_at(string *str, int index)
{
	if (index >= str->length)
		die("Error: Index out of bounds\n");

	if (index < 0){
		index +=str->length;
		if (index >= 0)
			return str->data[index];

		die("Error: Index out of bounds\n");
	}

	return str->data[index];
}

/* Other string function */
int cnet_strlen(string *s1)
{
	if (!s1)
		die("Error: Null Pointer\n");

	return s1->length;
}

string *cnet_str_lower(string *s)
{
	if (!s || !s->data)
		return s;

	string *s1 = clone_str(s);
	for(int i = 0; i<s->length;i++)
		s1->data[i] = tolower(s->data[i]);

	return s1;
}

string *cnet_str_upper(string *s)
{
	if (!s || !s->data)
		return s;

	string *s1 = clone_str(s);

	for(int i = 0; i<s1->length;i++){
		s1->data[i] = toupper(s->data[i]);
	}

	return s1;
}

/* [start, end) */
string *cnet_substring(string *s, int start, int end)
{
	if (start >= end)
		die("Error: Invalid range\n");
	string *s1 = create_str();
	s1->length = end - start;
	s1->data   = (char *)mem_alloc(s1->length);
	memcpy(s1->data, s->data+start, s1->length);

	return s1;

}

string *cnet_reverse_str(string *s)
{
	if (!s || !s->data)
		return s;

	string *s1 = clone_str(s);
	int len = s1->length;
	for(int i = 0; i<len;i++){
		s1->data[i] = s->data[len-i-1];
	}

	return s1;
}

void cpy_str(string *src, char *dst)
{
	// convert string * to char*
	memcpy(dst, src->data, src->length);
	dst[src->length] = '\0';
}

int cnet_str_atoi(string *s)
{
	char data[s->length+1];

	if (!s || !s->data)
		die("Error: Null Pointer\n");

	cpy_str(s, data);
	return atoi(data);
}

float cnet_str_atof(string *s)
{
	char data[s->length+1];

	if (!s || !s->data)
		die("Error: Null Pointer\n");

	cpy_str(s, data);

	return atof(data);
}

int cnet_find_char(string *s, char c)
{
	if (!s || !s->data)
		die("Error: Null Pointer\n");

	for(int i = 0; i<s->length;i++)
		if(s->data[i] == c)
			return i;

	return -1;
}

void print_cnet_str(string *s)
{
	for(int i=0; i<s->length;i++){
		printf("%c",cnet_char_at(s,i));
	}

}
