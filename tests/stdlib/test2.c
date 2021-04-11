#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"

int main()
{
	string *s1 = cnet_empty_str();

	string *s2 = cnet_new_str("hello", 5);

	assert(cnet_strcmp(s1, s2) != 0);

	string *s3 = cnet_strassign(s2);

	string *s4 = cnet_empty_str();

	assert(s1->length == s4->length);

	assert(cnet_strcmp(s1, s4) == 0);

	cnet_strmerge(s1, s2);

	assert(s1->length != s4->length);

	assert(cnet_strcmp(s1, s4) != 0);

	cnet_strcpy(s4, s3);

	assert(s1->length == s4->length);

	assert(cnet_strcmp(s1, s4) == 0);

	cnet_free(s1);
	cnet_free(s2);
	cnet_free(s3);
	cnet_free(s4);

	return 0;

}