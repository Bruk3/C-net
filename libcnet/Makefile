CC := gcc
CFLAGS := -Wall -g
OBJS := string.o utils.o io.o
DEPENDS := string.h utils.h io.h

libcnet.a: stdlib
	ar -cq libcnet.a *.o

stdlib: $(OBJS)

%.o: %.c $(DEPENDS)
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o *.a

all: clean stdlib

.PHONY: clean all
