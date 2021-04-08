CC := gcc
CFLAGS := -Wall -Werror -g -std=c11
OBJS := string.o utils.o

stdlib: $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $^

clean:
	rm -f *.o

all: clean stdlib

.PHONY: clean all