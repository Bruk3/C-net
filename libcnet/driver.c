
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include "utils.h"
#include "str.h"
#include "io.h"

cnet_array *parse_main_args(int argc, char **argv)
{
	cnet_array *args = cnet_init_array(8, String, argc, 0);
	string **data = (string **)args->data;
	for(int i = 0 ; i < argc; i++){
		string *new_str = cnet_new_str_nolen(argv[i]);
		cnet_strcpy(data[i], new_str);
		cnet_free(new_str);
	}

	return args;
}

int main(int argc, char **argv)
{
	cnet_array *string_arr = parse_main_args(argc-1, &argv[1]);
	int ret = user_main(string_arr);
	cnet_free(string_arr);
	return ret;
}