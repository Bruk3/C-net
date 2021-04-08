#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "utils.h"
#include "string.h"

int main()
{
	string *s1 = cnet_new_str();

	string *s2 = cnet_new_str_with_data("hello");

	cnet_copy_str(s1, s2);

	assert(s1->length == s2->length);

	assert(strcmp(s1->data, s2->data) == 0);

	assert(strcmp(s1->data, s2->data) == cnet_cmp_str(s1, s2));

	cnet_free_str(s1);
	cnet_free_str(s2);

	return 0;
}