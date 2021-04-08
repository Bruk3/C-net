#include <stdio.h>
#include <assert.h>
#include "string.h"

int main()
{
	string *s1 = cnet_create_str();

	string *s2 = cnet_create_str_with_data("hello");

	cnet_copy_str(s1, s2);

	assert(s1->length == s2->length);

	assert(strcmp(s1->data, s2->data));

	cnet_free_str(s1);
	cnet_free_str(s2);
	
	return 0;
}