#include <stdio.h>
#include <assert.h>
#include "../../../libcnet/string.h"

int main()
{
	string *s1;

	string *s2 = cnet_create_str_with_data("hello");

	string *s3 = cnet_create_str_with_data(" world");

	s1 = cnet_concat_str(s2, s3);

	assert(s1->length == s2->length);

	assert(strcmp(s1->data, s2->data));

	cnet_free_str(s1);
	cnet_free_str(s2);
	
	return 0;
}