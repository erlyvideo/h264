all: test


test: ebin/test.o ebin/encoder.o ebin/readjpeg.o
	gcc ebin/test.o ebin/encoder.o ebin/readjpeg.o -g -ljpeg -lx264 -lswscale -lavutil -o test


ebin/encoder.o: src/encoder.c
	gcc -c src/encoder.c -g -o ebin/encoder.o

ebin/readjpeg.o: src/readjpeg.c
	gcc -c src/readjpeg.c -g -o ebin/readjpeg.o

ebin/test.o: src/test.c
	gcc -c src/test.c -g -o ebin/test.o

clean:
	@rm -f test ebin/*.o