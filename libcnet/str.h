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

int slength(string *s1);

string *lower(string *s);

string *upper(string *s);

string *substring(string *s, int start, int end);

string *reverse(string *s);

void cnet_str_split(string *s,  string *delim, string **dest, int max);

// Bruk: This is deprecated because of the new one more extra byte allocation
// That extra byte is used to store a null terminator on demand.
/* void cpy_str(string *src, char *dst); */

int toint(string *s);

float tofloat(string *s);

int find_char(string *s, char c);

string *stringof(int n);

void print_cnet_str(string *s);

#endif
