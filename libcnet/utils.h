#ifndef _UTILS_H_
#define _UTILS_H_

#define DEFAULT_BUF_SIZE 4096
#define MAXPENDING 5
#define LISTEN 0
#define CONNECT 1

/* strings */ 
struct string {
	void (*cnet_free) (void *str);
	char *data;
	int length;
};

typedef struct string string;

/* files */ 
struct cnet_file {
	void (*cnet_free) (void *f);
	int fd;
	FILE *f;
};

typedef struct cnet_file cnet_file;

/* sockets */
struct cnet_socket {
	void (*cnet_free) (void *sock);
	int fd;
	int port;
	int type;
	struct sockaddr_in *addr;
};

typedef struct cnet_socket cnet_socket;

struct prot_type {
    int type;
	int prot;
};

typedef struct prot_type prot_type;

/* for casting purposes*/ 
struct cnet_io {
	void (*cnet_free) (void *ptr);
	int fd;
};

typedef struct cnet_io cnet_io;

void die(const char *message);

void *mem_alloc(int size);

void cnet_free(void *s);

#endif