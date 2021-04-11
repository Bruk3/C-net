#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"

void print_cnet_str(string *s)
{
	for(int i=0; i<s->length;i++){
		printf("%c",cnet_char_at(s,i));
	}

	printf("%s", "\n");

}

int main()
{
	string *s1 = cnet_new_str("hello", 5);

	printf("%s","s1: ");
	print_cnet_str(s1);	

	string *s2 = cnet_str_upper(s1);

	printf("%s","upper(s1) : ");
	print_cnet_str(s2);

	string *s3 = cnet_strcat(s1, s2);
	
	printf("%s","s1+upper(s1): ");
	print_cnet_str(s3);

	string *s4 = cnet_strmult(s3, s1->length);
	
	printf("%s","(s1+upper(s1))*3: ");
	print_cnet_str(s4);

	assert(cnet_strlen(s4) == (cnet_strlen(s1)+cnet_strlen(s2)) * cnet_strlen(s1));
	string *s5 = cnet_substring(s4, cnet_strlen(s1), cnet_strlen(s1)*2);
	
	printf("%s"," s5 -> s3[5:10]: ");
	print_cnet_str(s5);

	assert(cnet_strcmp(s2, s5) == 0);

	string *s6 = cnet_str_lower(s5);
	
	printf("%s","lower(s5): ");
	print_cnet_str(s6);

	assert(cnet_strcmp(s1, s6) == 0);

	string *s7 = cnet_reverse_str(s5);

	printf("%s","reverse(s5): ");
	print_cnet_str(s7);

	cnet_free(s1);
	cnet_free(s2);
	cnet_free(s3);
	cnet_free(s4);
	cnet_free(s5);
	cnet_free(s6);
	cnet_free(s7);

	return 0;
}
