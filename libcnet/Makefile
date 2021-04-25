CC := gcc
CFLAGS := -Wall -g
OBJS := string.o utils.o io.o
DEPENDS := string.h utils.h io.h

# clean up object files cause we don't need them
libcnet-main.a: stdlib driver.o
	ar -cq libcnet-main.a *.o
	rm -f *.o

libcnet.a: stdlib
	ar -cq libcnet.a *.o
	rm -f *.o

stdlib: $(OBJS)

%.o: %.c $(DEPENDS)
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o *.a

all: clean libcnet.a

.PHONY: clean all
