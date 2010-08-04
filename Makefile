all: test


test: ebin/test.o ebin/encoder.o
	gcc ebin/test.o ebin/encoder.o -g -lx264 -o test


ebin/encoder.o: src/encoder.c
	gcc -c src/encoder.c -g -o ebin/encoder.o

ebin/test.o: src/test.c
	gcc -c src/test.c -g -o ebin/test.o

clean:
	@rm -f test ebin/*.o