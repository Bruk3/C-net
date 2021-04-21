#ifndef _IO_H
#define _IO_H

#include "utils.h"

#define DEFAULT_BUF_SIZE 4096
#define MAXPENDING 5

/* IO types */
#define CNET_FILE_STDIN 0
#define CNET_FILE_STDOUT 1
#define CNET_FILE 2
#define CNET_SOCKET 3

/* Socket types */
#define LISTEN 0
#define CONNECT 1

cnet_file *cnet_open_file(string *filename, string *mode);

cnet_socket *cnet_listen_socket(int domain, int type, unsigned short port);

cnet_socket *cnet_accept_connection(cnet_socket *listener);

int cnet_write(void *ptr, string *s);

int cnet_nwrite(void *ptr, string *s, int length);

int writeln(void *ptr, string *s);

string *cnet_read(void *ptr);

string *cnet_nread(void *ptr, int size);

string *cnet_readln(void *ptr);

cnet_socket *cnet_connect_to_host(string *host_str, int port, int domain, int type);

int cnet_check_error(void *ptr);

#endif
