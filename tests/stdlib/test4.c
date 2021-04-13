#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"
#include "../../libcnet/io.h"

int main()
{
	string *s1 = cnet_new_str("42", 2);

	string *s2 = cnet_new_str("42.004", 6);

	assert(cnet_str_atoi(s1) == 42);

	printf("atof(\"42.004\"): %f\n", cnet_str_atof(s2));

	assert(cnet_find_char(s2, '0') == 3); 
	assert(cnet_find_char(s2, '.') == 2); 

	cnet_free(s1);
	cnet_free(s2);

	return 0;
}