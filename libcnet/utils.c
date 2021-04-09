#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "utils.h"


void die(const char *message)
{
	perror(message);
	exit(1);
}

void *mem_alloc(int size)
{
	void *mem = malloc(size);

	if (!mem)
		die("Could not allocate memory");

	return mem;
}

/* free yourself */ 
void cnet_free(void *s)
{
	cnet_cast *fs = (cnet_cast *)s;
	if(s)
		fs->cnet_free(s);
}

// static int serverSocket(unsigned short port)
// {
// 	int servSock;
// 	struct sockaddr_in servAddr;

// 	/* Create socket for incoming connections */ 
// 	if ((servSock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
// 		die("socket() failed");

// 	/* Construct local address structure */
// 	memset(&servAddr, 0, sizeof(servAddr));       /* Zero out structure */
// 	servAddr.sin_family = AF_INET;                /* Internet address family */
// 	servAddr.sin_addr.s_addr = htonl(INADDR_ANY); /* Any incoming interface */
// 	servAddr.sin_port = htons(port);              /* Local port */

// 	/* Bind to the local address */
// 	if (bind(servSock, (struct sockaddr *)&servAddr, sizeof(servAddr)) < 0)
// 		die("bind() failed");

// 	/* Mark the socket so it will listen for incoming connections */
// 	if (listen(servSock, MAXPENDING) < 0)
// 		die("listen() failed");

// 	return servSock;
// }

// FILE *cnet_open_file(char *filename, char *mode)
// {
// 	FILE *f = fopen(filename, mode);
// 	if (f == NULL) {
// 		fclose(fd);
// 		die("can't open file");
//     	}
// 	return f;
// }

// socket *cnet_create_socket(int servAddr, unsigned short port, char *prot)
// {
// 	if (strcmp(prot, "LISTEN"))
// 		return serverSocket(port);
	
// 	if(!strcmp(prot, "CONNECT")
// 		return NULL;

// 	int sock;
// 	if ((sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
// 		die("socket failed");

// 	memset(&serverAddr, 0, sizeof(serverAddr));
// 	serverAddr.sin_family = AF_INET;
// 	serverAddr.sin_addr.s_addr = inet_addr(serverIP);
// 	serverAddr.sin_port = htons(mdbPort);

// }

// int cnet_close_file(FILE *f)
// {
// 	int ret = fclose(f);
// 	if (!ret)
// 		fprintf(stderr, "error: %s\n", strerror(errno));
	
// 	return ret;
// }

// int cnet_close_socket(socket *s)
// {
// 	int ret = cnet_close_file(s->sock);
// 	free(s);
// 	return ret;
// }

// void connect(socket *s)
// {
// 	if(connect(s->sock, (struct sockaddr *)&s->dest_addr, sizeof(s->dest_addr)) < 0)
// 		die("Connection to socket failed");

// }

// int cnet_write(FILE *io, char *s, int length)
// {}

// int cnet_writeln(FILE *io, char *s, int length)
// {
// 	int n;
// 	length = min(length, s->length);
// 	n = write(io->fd, s->data, length);
// 	n += write(fd, "\n", 1);

// 	return n;
// }

// string *cnet_read(FILE *io)
// {}

// string *cnet_readln(FILE *io)
// {}

// string *cnet_scan()
// {}

// int cnet_println(string *s)
// {
// 	return cnet_writeln(stdout, s, s->length);
// }