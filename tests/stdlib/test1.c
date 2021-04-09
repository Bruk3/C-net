#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"

int main()
{
	string *s1 = cnet_empty_str();

	string *s2 = cnet_new_str("hello", strlen("hello"));

	cnet_free(s1);
	cnet_free(s2);
	
	return 0;
}