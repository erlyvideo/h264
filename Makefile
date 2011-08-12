NIFDIR := `erl -eval 'io:format("~s", [code:lib_dir(erts,include)])' -s init stop -noshell| sed s'/erlang\/lib\//erlang\//'`
NIF_FLAGS := `ruby -rrbconfig -e 'puts Config::CONFIG["LDSHARED"]'` -O3 -fPIC -fno-common -Wall

LDFLAGS=
ifeq ($(shell uname), Darwin)
LDFLAGS += -undefined dynamic_lookup
endif


all:  priv priv/x264.so priv/faac.so compile

priv:
	mkdir -p priv

compile:
	rm -f src/._* ._* c_src/._*
	ERL_LIBS=../erlyvideo/apps:/opt/erlyvideo/lib erl -make

priv/x264.so: c_src/x264.c 
	gcc -fPIC -shared $(LDFLAGS) -o $@ -lswscale c_src/x264.c -I $(NIFDIR) -g -lx264 -lavutil 

priv/faac.so: c_src/faac.c
	gcc -fPIC -shared $(LDFLAGS) -o $@ c_src/faac.c -I $(NIFDIR) -g -lfaac

clean:
	@rm -f test ebin/*.o ebin/*.so 
