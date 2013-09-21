NAME = iris
ENTRY = start_shortcut

all: clean compile

clean:
	rm -f src/*~
	rm -f src/*.beam
	rm -f src/modules/*~
	rm -f src/modules/*.beam
	rm -f *~
	rm -f ebin/*.beam
	rm -f *.dump

compile:
	rebar get-deps
	rebar compile

debug: clean all
	cd ebin
	erl -noshell -pa ebin deps/*/ebin -s $(NAME) $(ENTRY)

debug_sasl: clean all
	cd ebin
	erl -pa ebin deps/*/ebin -boot start_sasl -s $(NAME) $(ENTRY)
