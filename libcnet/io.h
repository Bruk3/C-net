#ifndef _IO_H
#define _IO_H

#include "utils.h"


cnet_file *cnet_open_file(string *filename, string *mode);

cnet_socket *cnet_listen_socket(int domain, int type, unsigned short port);

cnet_socket *cnet_accept_connection(cnet_socket *listener);

#endif