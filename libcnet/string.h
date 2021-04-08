#ifndef _STRING_H_
#define _STRING_H_

#define DEFAULT_LENGTH 20

/*string * can be casted to char * if needed */ 
struct string {
	char *data;
	int length;
};

typedef struct string string;

string *cnet_new_str();

string *cnet_new_str_with_data(char *data);

void cnet_free_str(string *str);

void cnet_copy_str(string *src, string *dst);

string *cnet_concat_str(string *s1, string *s2);

void cnet_merge_str(string *s1, string *s2);

int cnet_cmp_str(string *s1, string *s2);

char cnet_char_at(string *str, int index);

int cnet_str_length(string *s1);

string *cnet_str_lower(string *s);

string *cnet_str_upper(string *s);

string *cnet_substring(string *s, int start, int end);

string *cnet_reverse(string *s);

int cnet_str_atoi(string *s);

float cnet_str_atof(string *s);

int cnet_find_char(string *s, char c);

#endif