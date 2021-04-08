CC := gcc
CFLAGS := -Wall -Werror -g -std=c11
OBJS := string.o utils.o
DEPENDS := string.h utils.h 

stdlib: $(OBJS)

%.o: %.c $(DEPENDS)
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o

all: clean stdlib

.PHONY: clean all