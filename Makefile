NAME = iris
REBAR = $(shell command -v rebar || echo "./rebar")

all: clean compile control_tool

clean:
	$(REBAR) clean

compile:
	@echo $(REBAR)
	$(REBAR) compile

control_tool:
	erlc -I ./include -o ./iris_ctl ./src/control_tool/control_tool.erl
	sed -i '1s/^/#!\/usr\/bin\/env escript\n%%! -sname iris_ctl\n/' ./iris_ctl

deps:
	$(REBAR) get-deps

modules: deps
	erlc -I ./include -o ./ebin ./src/behaviours/*.erl
	erlc -I ./include -pa ./ebin -o ./ebin ./src/modules/*.erl

commands:
	erlc -I ./include -o ./ebin ./src/commands/*.erl

debug: compile
	cd ebin
	erl -name $(NAME) -noshell -pa ebin deps/*/ebin -config priv/iris.config -s $(NAME)

debug_sasl: all
	cd ebin
	erl -sname $(NAME) -pa ebin deps/*/ebin -boot start_sasl -config priv/iris.config -s $(NAME)

.PHONY: clean deps compile
