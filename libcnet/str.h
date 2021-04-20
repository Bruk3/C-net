#ifndef _STR_H_
#define _STR_H_
#include "utils.h"

#define DEFAULT_LENGTH 20

/*string * can be casted to char * if needed */


string *cnet_empty_str();

string *cnet_new_str(char *data, int length);

string *cnet_new_str_nolen(char* data);

string *cnet_strcpy(string *dst, string *src);

string *cnet_strassign(string *s);

string *cnet_strcat(string *s1, string *s2);

string *cnet_strmerge(string *s1, string *s2);

string *cnet_strmult(string *s, int mult);

int cnet_strcmp(string *s1, string *s2);

char cnet_char_at(string *str, int index);

int cnet_strlen(string *s1);

string *cnet_str_lower(string *s);

string *cnet_str_upper(string *s);

string *cnet_substring(string *s, int start, int end);

void cnet_str_split(string *s,  string *delim, string **dest);

string *cnet_reverse_str(string *s);

// Bruk: This is deprecated because of the new one more extra byte allocation
// That extra byte is used to store a null terminator on demand.
/* void cpy_str(string *src, char *dst); */

int cnet_str_atoi(string *s);

float cnet_str_atof(string *s);

int cnet_find_char(string *s, char c);

void print_cnet_str(string *s);

#endif