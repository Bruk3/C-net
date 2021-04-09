#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"
#include "../../libcnet/io.h"

int main()
{
	char line[DEFAULT_BUF_SIZE];
	char *fname = "test.c";
	string *filename = cnet_new_str(fname, strlen(fname));
	string *mode	 = cnet_new_str("a+", strlen("a+")); 
	cnet_file *file  = cnet_open_file(filename, mode);
	fread(line, 1, 10, file->f);
	string *s = cnet_new_str(line, 10);
	cnet_io io = {NULL, 1}; 
	cnet_writeln(&io, s);
	cnet_free(filename);
	cnet_free(mode);
	cnet_free(s);
	cnet_free(file);
}