#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"
#include "../../libcnet/io.h"

int main()
{
	int port 	 = 8000;
	char *host = "localhost";
	string *host_str = cnet_new_str_nolen(host);

	cnet_socket *client_sock = cnet_connect_to_host(host_str, port, 0, 0);

	if (cnet_check_error(client_sock))
		goto failed;

	printf("Connected to host %s\n", host);

	string *s = cnet_readln(client_sock);
	printf("%s", "Read from socket: ");
	print_cnet_str(s);
	cnet_free(s);

failed:
	cnet_free(host_str);
	cnet_free(client_sock);

	return 0;
}