all: clean compile

clean:
	rm -f src/*~
	rm -f src/*.beam
	rm -f *~
	rm -rf ebin/
	rm -f *.dump

compile:
	mkdir -p ebin/
	erl -pa ebin -make

debug: all
	cd ebin
	erl -noshell -pa ebin -s iris start
