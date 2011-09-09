include version.mk
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
	gcc -fPIC -shared $(LDFLAGS) -o $@ -lswscale c_src/x264.c -Ix264 -I $(NIFDIR) -g -lx264 -Lpriv -lavutil 

priv/faac.so: c_src/faac.c
	gcc -fPIC -shared $(LDFLAGS) -o $@ c_src/faac.c -I $(NIFDIR) -g -lfaac

clean:
	@rm -f test ebin/*.o priv/*.so ebin/*.so 


package: compile
	rm -rf tmproot
	mkdir -p tmproot/opt/erlyvideo/lib/h264-$(VERSION)/
	cp -r priv ebin src tmproot/opt/erlyvideo/lib/h264-$(VERSION)/
	cp encoder.preset tmproot/opt/erlyvideo/lib/h264-$(VERSION)/priv/
	cd tmproot && \
	fpm -s dir -t deb -n erly-h264 -d libfaac0 -d libswscale2 -d libavutil51 -d libx264-116 -v $(VERSION) -m "Max Lapshin <max@maxidoors.ru>" opt 
	mv tmproot/*.deb .

upload_package: 
	scp *$(VERSION)* erlyhub@git.erlyvideo.org:/apps/erlyvideo/debian/public/transcoding
	ssh erlyhub@git.erlyvideo.org "cd /apps/erlyvideo/debian ; ./update transcoding"


.PHONY: package upload_package

