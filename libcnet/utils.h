#ifndef _UTILS_H_
#define _UTILS_H_

#define DEFAULT_BUF_SIZE 4096
#define MAXPENDING 5

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
};

typedef struct cnet_file cnet_file;

/* sockets */
struct cnet_socket {
	void (*cnet_free) (void *sock);
	int fd;
	int port;
	struct sockaddr *addr;
	int sock_type;
	char buf[DEFAULT_BUF_SIZE];
	int buf_len;
};

typedef struct cnet_socket cnet_socket;

struct prot_type {
    int type;
	int prot;
};

typedef struct prot_type prot_type;

/* for casting purposes*/ 
struct cnet_cast {
	void (*cnet_free) (void *sock);
};

typedef struct cnet_cast cnet_cast;

void die(const char *message);

void *mem_alloc(int size);

void cnet_free(void *s);

#endif