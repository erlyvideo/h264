all: test


test: ebin/test.o ebin/encoder.o ebin/readjpeg.o ebin/rgb_yuv.o
	gcc ebin/test.o ebin/encoder.o ebin/readjpeg.o ebin/rgb_yuv.o -g -ljpeg -lx264 -o test


ebin/encoder.o: src/encoder.c
	gcc -c src/encoder.c -g -o ebin/encoder.o

ebin/readjpeg.o: src/readjpeg.c
	gcc -c src/readjpeg.c -g -o ebin/readjpeg.o

ebin/rgb_yuv.o: src/rgb_yuv.c
	gcc -c src/rgb_yuv.c -g -o ebin/rgb_yuv.o

ebin/test.o: src/test.c
	gcc -c src/test.c -g -o ebin/test.o

clean:
	@rm -f test ebin/*.o