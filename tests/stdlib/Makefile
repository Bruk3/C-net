CC := gcc

LIB_DIR = ../../libcnet
INCLUDES = -I$(LIB_DIR)

CFLAGS := -Wall -g -std=c11
VGFLAGS := --leak-check=full --track-origins=yes

OBJS := string.o utils.o
OBJS := $(addprefix $(LIB_DIR)/, $(OBJS));
DEPENDS := string.h utils.h
DEPENDS := $(addprefix $(LIB_DIR)/, $(DEPENDS));
CTESTS := cnet_test1

stdlib_tests: clean stdlib $(CTESTS)

cnet_%: %.o
	$(CC) $(CFLAGS) -o $@ $< $(OBJS)
	echo "Running " $@
	valgrind $(VGFLAGS) ./$@

%.o: .c $(DEPENDS)
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

stdlib: 
	cd $(LIB_DIR) && make all

clean:
	rm -f *.o cnet_* stdlib_tests_logs

all: clean stdlib_tests

.PHONY: stdlib clean all