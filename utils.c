#include <unistd.h>

int println(int fd, char *s, int len)
{
	write(fd, s, len);
}
