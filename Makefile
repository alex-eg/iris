NAME = iris
ENTRY = start_shortcut

all: clean compile

clean:
	rm -f src/*~
	rm -f src/*.beam
	rm -f src/modules/*~
	rm -f src/modules/*.beam
	rm -f *~
	rm -rf ebin/
	rm -f *.dump

compile:
	mkdir -p ebin/
	erl -pa ebin -make

debug: all
	cd ebin
	erl -noshell -pa ebin -s $(NAME) $(ENTRY)

debug_sasl: all
	cd ebin
	erl -pa ebin -boot start_sasl -s $(NAME) $(ENTRY)
