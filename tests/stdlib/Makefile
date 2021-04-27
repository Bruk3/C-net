CC := gcc

LIB_DIR = ../../libcnet
INCLUDES = -I$(LIB_DIR)

CFLAGS := -Wall -g
VGFLAGS := --leak-check=full --show-leak-kinds=all --track-origins=yes

OBJS := libcnet.a
OBJS := $(addprefix $(LIB_DIR)/, $(OBJS));
DEPENDS := string.h utils.h io.h
DEPENDS := $(addprefix $(LIB_DIR)/, $(DEPENDS));
# CTESTS := cnet_test1 cnet_test2 cnet_test3 cnet_test4 cnet_test5 cnet_test6 cnet_test7 cnet_test8 cnet_test9 cnet_test10
CTESTS := cnet_test8

stdlib_tests: clean stdlib $(CTESTS)

cnet_%: %.o stdlib
	$(CC) $(CFLAGS) -o $@ $< $(OBJS)
	# echo -e "Running" $@
	# valgrind $(VGFLAGS) ./$@

%.o: .c $(DEPENDS)
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

stdlib:
	cd $(LIB_DIR) && make all

clean:
	rm -rf *.o cnet_* stdlib_tests_logs .dSYM

all: clean stdlib_tests

.PHONY: stdlib clean all
