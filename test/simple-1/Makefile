CC = cc
OBJECTS = main.o program.o

all: main

$(OBJECTS):
	cc ${CFLAGS} -c -o $@ $(@:.o=.c)

main: $(OBJECTS)

clean:
	rm -f $(OBJECTS) main
