#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/stat.h> 
#include <errno.h>
#include <stdio.h>
#include "utils.h"
#include "str.h"

static int sock_domain[] = {AF_INET, AF_INET6};

static prot_type ptype[] = {
    {SOCK_STREAM, IPPROTO_TCP},
    {SOCK_DGRAM, IPPROTO_UDP}
};


static void cnet_close_file(FILE *f)
{
	if (fclose(f) < 0)
		fprintf(stderr, "error: %s\n", strerror(errno));

}

static void cnet_free_file(void *ptr)
{
    cnet_file *file = (cnet_file *)ptr;
    cnet_close_file(file->f);
    free(file);
}

static void cnet_free_socket(void *ptr)
{
    cnet_socket *s = (cnet_socket *)ptr;
	close(s->fd);
    
	free(s);
    
}

cnet_file *cnet_open_file(string *filename, string *mode)
{
    char fname[filename->length+1], md[mode->length+1];

    cpy_str(filename, fname);
    cpy_str(mode, md);

	FILE *f = fopen(fname, md);
	
    if (!f) {
		die("can't open file");
    }

    cnet_file *file = (cnet_file *)mem_alloc(sizeof(cnet_file));
    file->cnet_free = cnet_free_file;
    file->f         = f;
    file->fd        = fileno(f);

    if (file->fd < 0){
        die("Could not open file");
    }

	return file;
}

// string *cnet_read(void *ptr, int read_all, int size)
// {

// }

// string *cnet_readln(void *ptr)
// {
    
// }


int cnet_nwrite(void *ptr, string *s, int length)
{
    int n;
    cnet_io *io = (cnet_io *)ptr; 
    length = (length > s->length) ? s->length : length;
    n = write(io->fd, s->data, length);
    if (n != length){
        fprintf(stderr, "error: %s", strerror(errno));
        die("\n");
    }

    return n;
}

int cnet_write(void *ptr, string *s)
{
	return cnet_nwrite(ptr, s, s->length);
}

int cnet_writeln(void *ptr, string *s)
{
    int n, tmp;
    cnet_io *io = (cnet_io *)ptr; 
	n = write(io->fd, s->data, s->length);
    if (n != s->length){
        fprintf(stderr, "error: %s", strerror(errno));
        die("\n");
    }
    tmp = write(io->fd, "\n", 1);
    if (tmp != 1){
        fprintf(stderr, "error: %s", strerror(errno));
        die("\n");
    }

	return n + tmp;
}

int cnet_get_fd(cnet_file *file)
{
    return file->fd;
}

static cnet_socket *create_listener(int fd, int domain, unsigned short port)
{
    cnet_socket *sock   = (cnet_socket *)mem_alloc(sizeof(cnet_socket));
    
    sock->cnet_free = cnet_free_socket;
    sock->fd        = fd;
    sock->port      = port;
    sock->type      = LISTEN;
    sock->addr      = (struct sockaddr_in *)mem_alloc(sizeof(struct sockaddr_in));
    
    memset(sock->addr, 0, sizeof(struct sockaddr_in));
    sock->addr->sin_family = sock_domain[domain];
    sock->addr->sin_addr.s_addr = htonl(INADDR_ANY);
    sock->addr->sin_port   = htons(port);

    return sock;
}

static cnet_socket *create_connection_socket()
{
    cnet_socket *sock   = (cnet_socket *)mem_alloc(sizeof(cnet_socket));
    
    sock->cnet_free = cnet_free_socket;
    sock->type      = CONNECT;
    sock->addr      = (struct sockaddr_in *)mem_alloc(sizeof(struct sockaddr_in));

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

    fd = socket(sock_domain[domain], ptype[type].type, ptype[type].prot);
    if(fd < 0)
        goto failed;

    sock = create_listener(fd, domain, port);

    if(bind(sock->fd, (struct sockaddr *)sock->addr, sizeof(struct sockaddr_in)) < 0)
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
    if (listener->type != LISTEN)
        die("Non-Listener socket cannot accept connections.");

    int fd;
    unsigned int len;
    cnet_socket *conn_sock;
    
    conn_sock = create_connection_socket();
    len = sizeof(struct sockaddr_in);

    if ((fd = accept(listener->fd, (struct sockaddr *)conn_sock->addr, &len)) < 0)
        goto failed;

    conn_sock->fd = fd;

    goto out;

failed:
    conn_sock->cnet_free(conn_sock);
    die("Error accepting new connection\n");

out:
    return conn_sock;
}

/* client socket */ 
cnet_socket *cnet_connect_to_host(string *host_str, int port, int domain, int type)
{
    struct sockaddr_in server_addr;
    struct hostent *he;
    int fd;
    cnet_socket *conn_sock;
    char host[host_str->length+1];

   cpy_str(host_str, host);

    // get server ip from server name
    if ((he = gethostbyname(host)) == NULL)
        die("gethostbyname failed");

    char *serverIP = inet_ntoa(*(struct in_addr *)he->h_addr_list[0]);

    // create socket
    if ((fd = socket(sock_domain[domain], ptype[type].type, ptype[type].prot)) < 0) {
        die("socket failed");
    }

    conn_sock = create_connection_socket();
    conn_sock->fd = fd;
    server_addr = *conn_sock->addr;

    // construct server address
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = inet_addr(serverIP);
    server_addr.sin_port = htons(port);

    // connect
    if (connect(conn_sock->fd, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0){
        cnet_free(conn_sock);
        die("connect failed");
    }

    return conn_sock;
}

int cnet_get_socket_port(cnet_socket *sock)
{  
    return sock->port;
}