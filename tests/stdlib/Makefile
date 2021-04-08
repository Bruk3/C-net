CC := gcc
LIB_DIR = ../../libcnet
INCLUDES = -I$(LIB_DIR)
CFLAGS := -Wall -Werror -g -std=c11
CTESTS := test1 test2
DEPENDS := string.h utils.h
DEPENDS := $(addprefix $(LIB_DIR)/, $(DEPENDS));

stdlib_tests: $(CTESTS)

%: %.o
	$(CC) $(CFLAGS) -o $@ $<

%.o: %.c $(DEPENDS)
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

clean:
	rm -f *.o test*

all: clean stdlib_tests

.PHONY: clean all