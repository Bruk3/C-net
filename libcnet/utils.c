#include <unistd.h>

void println(int fd, char *s, int len)
{
	write(fd, s, len);
	write(fd, "\n", 1);
}
