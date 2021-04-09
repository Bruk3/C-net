#ifndef _UTILS_H_
#define _UTILS_H_

#define DEFAULT_BUF_SIZE 4096

/* strings */ 
struct string {
	void (*cnet_free) (void *str);
	char *data;
	int length;
};

typedef struct string string;

/* files */ 
struct file {
	void (*cnet_free) (void *f);
	int fd;
};

typedef struct file file;

/* sockets */
struct socket {
	void (*cnet_free) (void *sock);
	int fd;
	int port;
	struct sockaddr_in *addr;
	int sock_type;
	char buf[DEFAULT_BUF_SIZE];
};

typedef struct socket socket;

struct cnet_custom {
	void (*cnet_free) (void *sock);
};

typedef struct cnet_custom cnet_custom;

typedef void (*fn_def)(void *);

void die(const char *message);

void *mem_alloc(int size);

void cnet_free(void *s);

#endif