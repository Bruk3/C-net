#ifndef _IO_H
#define _IO_H

#include "utils.h"


cnet_file *cnet_open_file(string *filename, string *mode);

cnet_socket *cnet_listen_socket(int domain, int type, unsigned short port);

cnet_socket *cnet_accept_connection(cnet_socket *listener);

int cnet_write(void *ptr, string *s);

int cnet_nwrite(void *ptr, string *s, int length);

int cnet_writeln(void *ptr, string *s);

int cnet_get_fd(cnet_file *file);

#endif