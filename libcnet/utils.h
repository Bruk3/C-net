#ifndef _UTILS_H_
#define _UTILS_H_

/* strings */
struct string {
	void (*cnet_free) (void *);
	char *data;
	int length;
};

typedef struct string string;

/* files */
struct cnet_file {
	void (*cnet_free) (void *f);
	FILE *f;
	int io_type;
};

typedef struct cnet_file cnet_file;

/* sockets */
struct cnet_socket {
	void (*cnet_free) (void *sock);
	FILE *f;
	int io_type;
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
	FILE *f;
	int io_type;
};

typedef struct cnet_io cnet_io;

struct cnet_array {
	void (*cnet_free) (void *ptr);
	void *data;
	int length;
	int i_t;
	int type_t;
};

enum cnet_types {Num, Float, String, Complex};

typedef struct cnet_array cnet_array;

void die(const char *message);

void *mem_alloc(int size);

void cnet_free(void *s);

int user_main(cnet_array *args);
#endif
