#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "utils.h"
#include "str.h"


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
							(char *) mem_alloc(s->length + 1);

	return new_str;
}
static void __concat_str(char **s, string *s1, string *s2)
{
	*s = (char *) mem_alloc(s1->length + s2->length + 1);

	if(s1->data)
		memcpy(*s, s1->data, s1->length);
	if(s2->data)
		memcpy((*s) + s1->length, s2->data, s2->length);
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
	new_str->data		= (char *)mem_alloc(length + 1);

	memcpy(new_str->data, data, length);

	return new_str;
}

/* char *data must be null terminated */
string *cnet_new_str_nolen(char* data)
{
	int len = strlen(data);
	char *new_data = malloc(len);
	memcpy(new_data, data, len);
	return cnet_new_str(data, len);
}

/*(deep copy) eg.
 * string s1 = "Hi";
 * string s2 = "Hell0";
 * s1 = s2;
 * s3 = (s1 = s2);
 */
string *cnet_strcpy(string *dst, string *src)
{
	if (!src)
		die("[COMPILER ERROR] cnet_strcpy called from NULL source string\n");
	else if (!dst) {
		/* string a; a = "hello" */
		return cnet_strassign(src);
	}

	if (dst->data)
		free(dst->data);

	dst->length = src->length;
	dst->data = (char *) mem_alloc(src->length + 1);

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
		die("Error: String addtion Null Pointer\n");

	char *temp_data;
	__concat_str(&temp_data, s1, s2);

	string *new_str = create_str();

	new_str->length = s1->length+s2->length;
	new_str->data	= temp_data;

	return new_str;
}

/* Operator += */
string *cnet_strmerge(string *s1, string *s2)
{
	if (!s1 || !s2)
		die("Error: Null Pointer\n");

	char *temp_data;

	__concat_str(&temp_data, s1, s2);

	if (s1->data)
		free(s1->data);

	s1->length += s2->length;
	s1->data    = temp_data;

	return s1;
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
	new_str->data	= (char *) mem_alloc(new_str->length + 1);

	for (int i = 0; i < mult; i++)
		memcpy(&new_str->data[i*s->length], s->data, s->length);

	return new_str;
}

/* Operator == */
int cnet_strcmp(string *s1, string *s2)
{
	int min_length;

	if (!s1 && !s2)
		return 0;

	// Need to check s1 || s2 is not an empty string
	//  before null-terminating
	if (!s1->data && !s2->data)
		return 0;

	if (!s1->data || !s2->data)
		return -1;

	min_length = s1->length > s2->length ? s2->length : s1->length;


	// null-terminator is stored at the last extra byte alloated
	s1->data[s1->length] = '\0';
	s2->data[s2->length] = '\0';
	return !memcmp(s1->data, s2->data, min_length);
}

/* Operator [] */
char charat(string *str, int index)
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
int slength(string *s1)
{
	if (!s1)
		die("Error: Null Pointer\n");

	return s1->length;
}

string *lower(string *s)
{
	if (!s || !s->data)
		return s;

	string *s1 = clone_str(s);
	for(int i = 0; i<s->length;i++)
		s1->data[i] = tolower(s->data[i]);

	return s1;
}

string *upper(string *s)
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
string *substring(string *s, int start, int end)
{
	if (start >= end)
		die("Error: Invalid range\n");
	string *s1 = create_str();
	s1->length = end - start;
	s1->data   = (char *)mem_alloc(s1->length + 1);
	memcpy(s1->data, s->data+start, s1->length);

	return s1;

}

string *reverse(string *s)
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

// Bruk: This is deprecated because of the new one more extra byte allocation
// That extra byte is used to store a null terminator on demand.
/*
void cpy_str(string *src, char *dst)
{
	// convert string * to char*
	memcpy(dst, src->data, src->length);
	dst[src->length] = '\0';
}
*/

int toint(string *s)
{
	if (!s || !s->data)
		die("Error: Null Pointer\n");

	s->data[s->length] = '\0';
	return atoi(s->data);
}

float tofloat(string *s)
{
	if (!s || !s->data)
		die("Error: Null Pointer\n");

	s->data[s->length] = '\0';
	return atof(s->data);
}


void split(string *s,  string *delim, cnet_array *dest)
{
	if (!s || !s->data)
		die("Error: Null Pointer\n");

	char *buf = mem_alloc(s->length);
	memcpy(buf, s->data, s->length);

	s->data[s->length] = '\0';
	delim->data[delim->length] = '\0';
	char *token = strtok(buf, delim->data);

	int i = 0;
	string **d = dest->data;
	// printf("before loop\n");
	while (token != NULL && i < dest->length){
		// printf("token: %s\n", token);
		d[i] = cnet_new_str_nolen(token);
		token = strtok(NULL, delim->data);
		i += 1;
	}
	// printf("after loop\n");

}

string *user_soi(int num)
{
	char buf[12]; //
	sprintf(buf, "%d", num);
	string *s = cnet_new_str_nolen(buf);
	return s;
}

int find_char(string *s, char c)
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
	if (!s || !s->data)
		return;

	s->data[s->length] = '\0';
	printf("%s", s->data);


}

