#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"

int main()
{
	string *s1 = cnet_empty_str();

	string *s2 = cnet_new_str("hello", 5);

	cnet_strcpy(s1, s2);

	assert(s1->length == s2->length);

	assert(strcmp(s1->data, s2->data) == 0);

	// assert(strcmp(s1->data, s2->data) == cnet_strcmp(s1, s2));

	// string *s3 = cnet_new_str(" ", 1);
	// string *s4 = cnet_str_upper(s1);

	// string *s5 = cnet_strcat(s4, s3);
	// string *s6 = cnet_strcat(s5, s2);
	
	// printf("%s\n%s\n",s5->data, s6->data);

	cnet_free(s1);
	cnet_free(s2);
	// cnet_free(s3);
	// cnet_free(s4);
	// cnet_free(s5);
	// cnet_free(s6);

	return 0;
}