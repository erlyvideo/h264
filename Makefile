NIFDIR := `erl -eval 'io:format("~s", [code:lib_dir(erts,include)])' -s init stop -noshell| sed s'/erlang\/lib\//erlang\//'`
NIF_FLAGS := `ruby -rrbconfig -e 'puts Config::CONFIG["LDSHARED"]'` -O3 -fPIC -fno-common -Wall

MPEG2 := -lmpeg2
MPEG2 := libmpeg2/*.o

all:  ebin/ems_video.so compile

compile:
	ERL_LIBS=../erlyvideo/apps erl -make

ebin/ems_video.so: src/ems_video.c src/ems_x264.c src/ems_mpeg2.c 
	gcc -c -o ebin/ems_video.o src/ems_video.c -I $(NIFDIR)
	gcc -shared -undefined dynamic_lookup -o $@ ebin/libswscale.a ebin/ems_video.o -g ebin/libx264.a -lavutil $(MPEG2)




ebin/readjpeg.o: src/readjpeg.c
	gcc -c src/readjpeg.c -g -o ebin/readjpeg.o

clean:
	@rm -f test ebin/*.o
