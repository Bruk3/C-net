CC := gcc

LIB_DIR = ../../libcnet
INCLUDES = -I$(LIB_DIR)

CFLAGS := -Wall -g
VGFLAGS := --leak-check=full --track-origins=yes

OBJS := string.o utils.o  io.o
OBJS := $(addprefix $(LIB_DIR)/, $(OBJS));
DEPENDS := string.h utils.h io.h
DEPENDS := $(addprefix $(LIB_DIR)/, $(DEPENDS));
CTESTS := cnet_test1 cnet_test2 cnet_test3 cnet_test5

stdlib_tests: clean stdlib $(CTESTS)

cnet_%: %.o stdlib
	$(CC) $(CFLAGS) -o $@ $< $(OBJS)
	echo -e "Running" $@
	valgrind $(VGFLAGS) ./$@

%.o: .c $(DEPENDS)
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

stdlib:
	cd $(LIB_DIR) && make all

clean:
	rm -f *.o cnet_* stdlib_tests_logs

all: clean stdlib_tests

.PHONY: stdlib clean all