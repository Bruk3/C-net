CC := gcc
INCLUDES = -I../../libcnet
CFLAGS := -Wall -Werror -g -std=c11 $(INCLUDES)
CTESTS := cnet_test1 cnet_test2

stdlib_tests: $(CTESTS)

cnet_%: %.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f *.o cnet_*

all: clean stdlib_tests

.PHONY: clean all