#include <unistd.h>

int println(int fd, char *s, int len)
{
	int n;
	n = write(fd, s, len);
	if (n != len)
		return n;
	n += write(fd, "\n", 1);

	return n;
}
