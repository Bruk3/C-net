#ifndef _STR_H_
#define _STR_H_
#include "utils.h"

#define DEFAULT_LENGTH 20

/*string * can be casted to char * if needed */ 


string *cnet_empty_str();

string *cnet_new_str(char *data, int length);

void cnet_strcpy(string *dst, string *src);

string *cnet_strcat(string *s1, string *s2);

void cnet_strmerge(string *s1, string *s2);

int cnet_strcmp(string *s1, string *s2);

char cnet_char_at(string *str, int index);

int cnet_strlen(string *s1);

string *cnet_str_lower(string *s);

string *cnet_str_upper(string *s);

string *cnet_substring(string *s, int start, int end);

string *cnet_reverse(string *s);

int cnet_str_atoi(string *s);

float cnet_str_atof(string *s);

int cnet_find_char(string *s, char c);

#endif