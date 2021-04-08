#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "utils.h"
#include "string.h"

static string *create_str()
{
	string *new_str = (string *) mem_alloc(sizeof(string));

	new_str->length 	= 0;
	new_str->data		= NULL;

	return new_str;
}

static string *clone_str(string *s)
{
	string *new_str = (string *) mem_alloc(sizeof(string));

	new_str->length 	= s->length;
	new_str->data		= s->length == 0 ? NULL :
					(char *) mem_alloc(sizeof(s->length+1));

	return new_str;
}
static void __concat_str(char *s, string *s1, string *s2)
{
	s = (char *) mem_alloc(s1->length+s2->length+1);
	strncpy(s, s1->data, s1->length);
	strncpy(s+s1->length, s2->data, s2->length+1);

}

/* Constructors */ 
string *cnet_new_str()
{
	return create_str();
}

string *cnet_new_str_with_data(char *data)
{
	string *new_str =  create_str();
	new_str->length = strlen(data);
	new_str->data	= (char *)mem_alloc(new_str->length+1);
	strncpy(new_str->data, data, new_str->length+1);

	return new_str;
}

/* Destructor */
void cnet_free_str(string *str)
{
	if (str->length)
		free(str->data);
	free(str);
}

/* operator '=' (deep copy) */
void cnet_copy_str(string *src, string *dst)
{
	if (dst->length < src->length){
		free(dst->data);
		dst->data = (char *) mem_alloc(src->length+1);
	}

	strncpy(dst->data, src->data, dst->length+1);
	dst->length = src->length;
}

/* Operator + */
string *cnet_concat_str(string *s1, string *s2)
{
	string *new_str = create_str();
	new_str->length = s1->length+s2->length;
	__concat_str(new_str->data, s1, s2);

	return new_str;
}

/* Operator += */ 
void cnet_merge_str(string *s1, string *s2)
{
	char *temp_data = (char *) mem_alloc(s2->length+s1->length+1);	
	__concat_str(temp_data, s1, s2);
	if (s1->length)
		free(s1->data);
	
	s1->length += s2->length;
	s1->data    = temp_data;
}

/* Operator ==, >, < */ 
int cnet_cmp_str(string *s1, string *s2)
{
	return strcmp(s1->data, s2->data);
}

/* Operator [] */ 
char cnet_char_at(string *str, int index)
{
	if (index >= str->length)
		return '\0';
 
	return str->data[index];
}

/* Other string function */ 
int cnet_str_length(string *s1)
{
	return s1->length;
}

string *cnet_str_lower(string *s)
{
	string *s1 = clone_str(s);
	for(int i = 0; i<s->length;i++)
		s1->data[i] = tolower(s->data[i]);
	
	s1->data[s1->length] = '\0';

	return s1;
}

string *cnet_str_upper(string *s)
{
	string *s1 = clone_str(s);
	for(int i = 0; i<s->length;i++)
		s1->data[i] = toupper(s->data[i]);
	
	s1->data[s1->length] = '\0';

	return s1;
}

/* [start, end) */ 
string *cnet_substring(string *s, int start, int end)
{
	string *s1 = create_str();
	s1->length = end - start;
	s1->data   = (char *)mem_alloc(s1->length+1);
	strncpy(s1->data, s->data+start, s1->length);
	s1->data[s->length] = '\0';

	return s1;

}

string *cnet_reverse(string *s)
{	
	string *s1 = clone_str(s);
	int len = s1->length;
	for(int i = 0; i<len/2;i++)
		s1->data[i] = s->data[len-i-1];
	
	s1->data[s1->length] = '\0';

	return s1;
}

int cnet_str_atoi(string *s)
{
	return atoi(s->data);
}

float cnet_str_atof(string *s)
{
	return atof(s->data);
}

int cnet_find_char(string *s, char c)
{
	for(int i = 0; i<s->length;i++)
		if(s->data[i] == c)
			return i;

	return -1;
}
