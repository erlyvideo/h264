NIFDIR := `erl -eval 'io:format("~s", [code:lib_dir(erts,include)])' -s init stop -noshell| sed s'/erlang\/lib\//erlang\//'`
NIF_FLAGS := `ruby -rrbconfig -e 'puts Config::CONFIG["LDSHARED"]'` -O3 -fPIC -fno-common -Wall

LDFLAGS=""
ifeq ($(shell uname), Darwin)
LDFLAGS += -undefined dynamic_lookup
endif


all:  ebin/ems_video.so ebin/ems_sound2.so compile

compile:
	ERL_LIBS=../erlyvideo/apps:/opt/erlyvideo/lib erl -make

ebin/ems_video.so: src/ems_video.c src/ems_x264.c 
	gcc -fPIC -c -o ebin/ems_video.o src/ems_video.c -I $(NIFDIR)
	gcc -fPIC -shared $(LDFLAGS) -o $@ -lswscale ebin/ems_video.o -g -lx264 -lavutil 

ebin/ems_sound2.so: src/ems_sound2.c src/ems_mpg123.c src/ems_faac.c
	gcc -fPIC -c -o ebin/ems_sound2.o src/ems_sound2.c -I $(NIFDIR)
	gcc -fPIC -shared -undefined dynamic_lookup -o $@ ebin/ems_sound2.o -g -lmpg123 -lfaac

clean:
	@rm -f test ebin/*.o ebin/*.so libmpeg2/*.o libmpeg2/*.d
