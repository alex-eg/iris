NAME = iris

all: clean compile

clean:
	rebar clean

compile:
	rebar compile

deps:
	rebar get-deps

modules: deps
	erlc -I ./include -o ./ebin ./src/behaviours/*.erl
	erlc -I ./include -pa ./ebin -o ./ebin ./src/modules/*.erl

commands:
	erlc -I ./include -o ./ebin ./src/commands/*.erl

debug: compile
	cd ebin
	erl -noshell -pa ebin deps/*/ebin -config priv/iris.config -s $(NAME)

debug_sasl: all
	cd ebin
	erl -pa ebin deps/*/ebin -boot start_sasl -config priv/iris.config -s $(NAME)

.PHONY: clean deps
