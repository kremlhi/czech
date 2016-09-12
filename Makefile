CFLAGS += -ansi -pedantic -Wall -ggdb
ERLC   ?= erlc

SRC     = $(wildcard src/*.erl)
BEAM    = $(SRC:src/%.erl=ebin/%.beam)
MODULES = $(SRC:src/%.erl=%,)
APP_SRC = $(wildcard src/*.app.src)
APP     = $(APP_SRC:src/%.app.src=ebin/%.app)
C_SRC   = $(wildcard c_src/*.c)
AOUT    = $(C_SRC:c_src/%.c=priv/%)
DSYM    = $(C_SRC:c_src/%.c=priv/%.dSYM)

ebin/%.beam: src/%.erl
	$(ERLC) -W -o ebin +debug_info $<

ebin/%.app: src/%.app.src
	sed -e 's/{modules, *\[/&$(MODULES)/' -e 's/,\]/]/' $< >$@

priv/%: c_src/%.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

all: $(BEAM) $(APP) $(AOUT)

dialyze:
	dialyzer -Wunderspecs --src src

clean:
	rm -f $(BEAM) $(APP) $(AOUT)
	rm -rf $(DSYM)
