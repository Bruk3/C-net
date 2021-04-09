#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "utils.h"

static int[] sock_domain = {AF_INET, AF_INET6};

static prot_type[] ptype = {
    {SOCK_STREAM, IPPROTO_TCP},
    {SOCK_DGRAM, IPPROTO_UDP}
};


int cnet_close_socket(cnet_socket *s)
{
	int ret = cnet_close_file(s->sock);
    if (s->buf)
        free(buf);

	free(s);
	
    return ret;
}

int cnet_close_file(file *f)
{
	int ret = fclose(f->fd);
	if (!ret)
		fprintf(stderr, "error: %s\n", strerror(errno));
	
    free(f);
	return ret;
}

int cnet_open_file(char *filename, char *mode)
{
	FILE *f = fopen(filename, mode);
	
    if (f == NULL) {
		fclose(fd);
		die("can't open file");
    }

	return f;
}

string *cnet_read()
{}

string *cnet_readln()
{}

int cnet_write()
{}

int cnet_writeln()
{}

static cnet_socket *cnet_new_socket(int fd, int domain, unsigned short port)
{
    struct sockaddr_in saddr;
    cnet_socket *sock   = (cnet_socket *)mem_alloc(sizeof(cnet_socket));
    
    sock->cnet_free = cnet_close_socket;
    sock->fd        = fd;
    sock->buf_len   = DEFAULT_BUF_SIZE;
    sock->buf       = (char *)mem_alloc(DEFAULT_BUF_SIZE);
    sock->addr      = (struct sockaddr *)mem_alloc(sizeof(saddr)) 

    saddr = (struct sockaddr_in)(*sock->addr);
    memset(&saddr, 0, sizeof(saddr));
    saddr.sin_family = sock_domain[domain];
    saddr.sin_addr.s_addr = htonl(INADDR_ANY);
    saddr.sin_port   = htons(port);

    return sock;
}

/*
 *
 * sock_type: 0 for server socket, 1 for client socket
 *
 */
cnet_socket *cnet_listen_socket(int domain, int type, unsigned short port)
{
    int fd;
    cnet_socket *sock;

    fd = socket(sock_domain[domain], ptype[type]->type, ptype[type]->prot);
    if(fd < 0)
        goto failed;

    sock = cnet_new_socket(fd, domain, port);

    if(bind(sock->fd, sock->addr, sizeof(struct sockaddr_in)) < 0)
        goto cleanup;
    
    if (listen(sock->fd, MAXPENDING) < 0)
        goto cleanup;

    goto out;

cleanup:
    sock->cnet_free(sock);

failed:
    die("Socket creation failed\n");

out:
    return sock;
}

cnet_socket *cnet_accept_connection(cnet_socket *listener)
{

}

/* client socket */ 
cnet_socket *cnet_connect_socket(string *host, int port)
{

}