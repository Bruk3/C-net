#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"
#include "../../libcnet/io.h"

int main()
{
	char line[DEFAULT_BUF_SIZE];
	string *filename = cnet_new_str("test3.c", strlen("test3.c"));
	string *mode	 = cnet_new_str("r", strlen("r")); 
	cnet_file *file = cnet_open_file(filename, mode);
	fread(line, 1, 10, file->f);
	string *s = cnet_new_str(line, 10);

	cnet_free(s);
	cnet_free(file);
}