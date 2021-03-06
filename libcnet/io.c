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

static int sock_domain[] = {AF_INET, AF_INET6};

static prot_type ptype[] = {
    {SOCK_STREAM, IPPROTO_TCP},
    {SOCK_DGRAM, IPPROTO_UDP}
};


static void cnet_close_file(FILE *f)
{
	if (!f && (fclose(f) < 0))
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
	if (s->type == LISTEN)
        close(s->fd);
    else
        fclose(s->f);

    free(s->addr);
	free(s);

}

static int inline check_socket_type(cnet_io *io)
{
	if (io == NULL) {
		fprintf(stderr, "read/write operation attempted on uninitialized file/socket\n");
		exit(1);
	}

	if(io->io_type == CNET_SOCKET &&
			((cnet_socket *)io)->type == LISTEN){
		fprintf(stderr, "error: %s", "Cannot write to listening socket");
		return 1;
	}

	return 0;
}

static void cnet_strmerge_custom(string *s, char *buf, int len)
{
    string temp = {NULL, buf, len};

    cnet_strmerge(s, &temp);

}

cnet_file *user_fopen(string *fname, string *mode)
{
    fname->data[fname->length] = '\0';
    mode->data[mode->length] = '\0';

    FILE *f = fopen(fname->data, mode->data);

    if (!f) {
	perror("can't open file");
        return NULL;
    }

    cnet_file *file = (cnet_file *)mem_alloc(sizeof(cnet_file));
    file->cnet_free = cnet_free_file;
    file->f         = f;
    file->io_type   = CNET_FILE;

    // Bruk: only needs to be different from 0 and 1 which are allocated
    // for STDIN and STDOUT;
    file->io_type   = 2;

	return file;
}

string *cnet_nread(void *ptr, int size)
{
    int n;
    cnet_io *io = (cnet_io *)ptr;
    string *res = cnet_empty_str();

    if (check_socket_type(io))
        return res;

    if (io->io_type == CNET_FILE_STDIN){
	    io->f = stdin;
    }

    int buf_size = (DEFAULT_BUF_SIZE > size) ? size : DEFAULT_BUF_SIZE;

    char buf[buf_size];

    // printf("in cnet_nread before while loop\n");
    while(size > 0 && (n = fread(buf, 1, buf_size, io->f)) > 0){
        // printf("size: %d", size);
        cnet_strmerge_custom(res, buf, n);
        size -= n;
        buf_size = (DEFAULT_BUF_SIZE > size) ? size : DEFAULT_BUF_SIZE;
    }

    if (ferror(io->f)){
        cnet_free(res);
        perror("fread failed");
    }

    return res;
}

string *readall(void *ptr)
{

    int n;
    cnet_io *io = (cnet_io *)ptr;

    string *res = cnet_empty_str();
    if (check_socket_type(io))
        return res;

    if (io->io_type == CNET_FILE_STDIN)
	    io->f = stdin;

    int buf_size = 1;
    char buf[buf_size];

    // printf("starting readall\n");
    while((n = fread(buf, 1, buf_size, io->f)) > 0){
        cnet_strmerge_custom(res, buf, n);
        // printf("[buf]: %s\n", buf);
    }

    if (ferror(io->f)){
        cnet_free(res);
        perror("fread failed");
    }

    return res;

}

/* Deprecated */
// static int find_nl_index(char *buf, int n)
// {
//     for (int i =0; i < n-1; i++){
//         if(buf[i] == '\n' || (buf[i] == '\r' && buf[i+1] == '\n'))
//             return i+1;
//     }

//     if(buf[n-1] == '\n')
//         return n;

//     return n+1;

// }

string *cnet_read_until(void *ptr, char *delim, int len)
{
    cnet_io *io = (cnet_io *)ptr;

    if (check_socket_type(io))
        return NULL;

    if (io->io_type == CNET_FILE_STDIN)
	    io->f = stdin;

    string *res = cnet_empty_str();
    int buf_size = DEFAULT_BUF_SIZE;
    int found = 0, curr = 0, n = 0;
    char buf[buf_size];
    int total = 0; // temp var for testing

    if (check_socket_type(io))
        die("cannot read on a listening socket");

    while (!found) {
        if (curr + len >= buf_size) {
            cnet_strmerge_custom(res, buf, curr);
            curr = 0;
            continue;
        }

        n = fread(buf + curr, 1, len, io->f);
        total += n; // never reset to 0 unlike curr
        if (n < len) {
            cnet_strmerge_custom(res, buf, curr + n);
            break;
        }

        // n = len (the delimiter has been found, break and return)
        if (memcmp(buf + curr, delim, len) == 0){
            cnet_strmerge_custom(res, buf, curr + n);
            break;
        }

        curr += len;
    }


    if (ferror(io->f)) {
        cnet_free(res);
        die("read_until failed");
    }

    return res;
}

string *readln(void *ptr)
{
    return cnet_read_until(ptr, "\n", 1);
}


/*     // while((n = fread(buf, 1, 1, io->f)) > 0){ */
/*     //     printf("in the loop trying to read\n"); */
/*     //     idx = find_nl_index(buf, n); */
/*     //     cnet_strmerge_custom(res, buf, ((idx < n)?idx:n)); */
/*     //     if (idx <= n) */
/*     //         break; */
/*     // } */

/*     // if (ferror(io->f)){ */
/*     //     cnet_free(res); */
/*     //     perror("fread failed"); */
/*     // } */

/*     // return res; */

/* } */



int nwrite(void *ptr, string *s, int length)
{
    int n;
    cnet_io *io = (cnet_io *)ptr;

    if (!ptr)
	    die("write/writeln called on invalid socket/file");

    if (check_socket_type(io))
        return 0;

    if (io->io_type == CNET_FILE_STDOUT)
	    io->f = stdout;

    length = (length > s->length) ? s->length : length;
    n = fwrite(s->data, 1, length, io->f);

    if (ferror(io->f)) {
        perror("fwrite failed");
    }

    return n;
}

int cnet_write(void *ptr, string *s)
{
	return nwrite(ptr, s, s->length);
}

int writeln(void *ptr, string *s)
{
    int n;
    string nl = {NULL, "\n", 1};
    if (s == NULL)
	    die("ptr is NULL in writeln");
    n  = nwrite(ptr, s, s->length);
    n += nwrite(ptr, &nl, nl.length);

	return n;
}

static cnet_socket *create_listener(int fd, int domain, unsigned short port)
{
    cnet_socket *sock   = (cnet_socket *)mem_alloc(sizeof(cnet_socket));

    sock->cnet_free = cnet_free_socket;
    sock->io_type   = CNET_SOCKET;
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
    sock->io_type   = CNET_SOCKET;
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
    perror("Socket creation failed\n");
    return NULL;

out:
    return sock;
}

cnet_socket *naccept(cnet_socket *listener)
{
    if (listener->type != LISTEN){
        perror("Non-Listener socket cannot accept connections.");
        return NULL;
    }

    int fd;
    unsigned int len;
    cnet_socket *conn_sock;

    conn_sock = create_connection_socket();
    len = sizeof(struct sockaddr_in);

    if ((fd = accept(listener->fd, (struct sockaddr *)conn_sock->addr, &len)) < 0)
        goto failed;

    conn_sock->fd = fd;
    conn_sock->f  = fdopen(fd, "w+");
    if(!conn_sock->f)
        goto failed;

    goto out;

failed:
    cnet_free(conn_sock);
    perror("Error accepting new connection\n");
    return NULL;

out:
    return conn_sock;
}

cnet_socket *user_nopen(string *host, int port, string *prot, string *type)
{
    int protocol;

    prot->data[prot->length] = '\0';

    if (strcasecmp(prot->data, "tcp") == 0){
        protocol = 0;
    } else if (strcasecmp(prot->data, "udp") == 0){
        protocol = 1;
    } else {
        die("Unknown transport protocol passed to nopen");
    }

    type->data[type->length] = '\0';

    if (strcasecmp(type->data, "listen") == 0){
        return cnet_listen_socket(0, protocol, port);
    } else if (strcasecmp(type->data, "connect") == 0){
        return cnet_connect_to_host(host, port, 0, protocol);
    } else {
        die("Invalid socket type passed to nopen");
    }

    return NULL;
}

/* client socket */
cnet_socket *cnet_connect_to_host(string *host, int port, int domain, int type)
{
    struct sockaddr_in server_addr;
    struct hostent *he;
    int fd;
    cnet_socket *conn_sock;

   host->data[host->length] = '\0';

    // get server ip from server name
    // printf("%s\n", host->data);
    if ((he = gethostbyname(host->data)) == NULL){
        perror("gethostbyname failed");
        return NULL;
    }

    char *serverIP = inet_ntoa(*(struct in_addr *)he->h_addr_list[0]);

    // create socket
    if ((fd = socket(sock_domain[domain], ptype[type].type, ptype[type].prot)) < 0) {
        perror("socket failed");
        return NULL;
    }

    conn_sock = create_connection_socket();
    conn_sock->fd = fd;
    conn_sock->f  = fdopen(fd, "w+");
    if(!conn_sock->f)
        goto failed;

    server_addr = *conn_sock->addr;

    // construct server address
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = inet_addr(serverIP);
    server_addr.sin_port = htons(port);

    // connect
    if (connect(conn_sock->fd, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0)
        goto failed;

    goto out;

failed:
    cnet_free(conn_sock);
    perror("connect failed");
    return NULL;

out:
    return conn_sock;
}

int cnet_get_socket_port(cnet_socket *sock)
{
    return sock->port;
}

int error(void *ptr)
{
    return (cnet_io *)ptr == NULL;
}

cnet_file cnet_stdin_ac = {
    .cnet_free = NULL,
    .f         = NULL,
    .io_type    = CNET_FILE_STDIN
};

cnet_file *cnet_stdin = &cnet_stdin_ac;

cnet_file cnet_stdout_ac = {
    .cnet_free = NULL,
    .f         = NULL,
    .io_type    = CNET_FILE_STDOUT
};

cnet_file *cnet_stdout = &cnet_stdout_ac;
