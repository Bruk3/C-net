#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"
#include "../../libcnet/io.h"

int main()
{
	cnet_socket *listener = cnet_listen_socket(0, 0, 8090);

	printf("%s\n", "Listener socket created");
	cnet_socket *conn_sock = cnet_accept_connection(listener);

	string *s = cnet_new_str_nolen("Connection accepted");

	printf("%s\n","Connection accepted");

	cnet_writeln(conn_sock, s);

	cnet_free(listener);
	cnet_free(conn_sock);
	cnet_free(s);

	return 0;
}