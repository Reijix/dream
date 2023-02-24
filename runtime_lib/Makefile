.PHONY: all
all: dreamlib.a

startup.o: startup.s
	as startup.s -o startup.o

library_functions.o: library.s
	as library.s -o library.o

dreamlib.a: startup.o library.o
	ar crs dreamlib.a startup.o library.o

