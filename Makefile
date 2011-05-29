NIFDIR := `erl -eval 'io:format("~s", [code:lib_dir(erts,include)])' -s init stop -noshell| sed s'/erlang\/lib\//erlang\//'`
NIF_FLAGS := `ruby -rrbconfig -e 'puts Config::CONFIG["LDSHARED"]'` -O3 -fPIC -fno-common -Wall

all: test ebin/ems_video.so compile

compile:
	ERL_LIBS=../erlyvideo/apps erl -make

ebin/ems_video.so: src/ems_video.c src/ems_jpeg.c src/ems_x264.c src/ems_mpeg2.c test
	gcc -c -o ebin/ems_video.o src/ems_video.c -I $(NIFDIR)
	cd libmpeg2 && make
	gcc -shared -undefined dynamic_lookup -o $@ ebin/encoder.o ebin/libswscale.a ebin/readjpeg.o ebin/ems_video.o -g -ljpeg -lx264 -lavutil libmpeg2/libmpeg2/.libs/libmpeg2.a



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
