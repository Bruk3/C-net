#ifndef _UTILS_H_
#define _UTILS_H_

#include "string.h"

#define DEFAULT_BUF_SIZE 4096

/* files/sockets */
struct socket {
	int sock;
	int port;
	struct sockaddr_in *bind_addr;
	int dest_addr;
	int sock_type;
	char buf[DEFAULT_BUF_SIZE];
};

typedef struct socket socket;

void die(const char *message);

void *mem_alloc(int size);

#endif