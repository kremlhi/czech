CFLAGS += -ansi -pedantic -Wall -ggdb

ERL_SRC	= $(wildcard src/*.erl)
ELISP	= $(wildcard elisp/*.el)
BEAMS	= $(ERL_SRC:src/%.erl=ebin/%.beam)
MODULES = $(ERL_SRC:src/%.erl=%,)
APP_SRC = $(wildcard src/*.app.src)
APP	= $(APP_SRC:src/%.app.src=ebin/%.app)
ELC	= $(ELISP:%.el=%.elc)
SRC	= $(wildcard c_src/*.c)
AOUT	= $(SRC:c_src/%.c=priv/%)
DSYM	= $(SRC:c_src/%.c=priv/%.dSYM)

ERLC	= erlc
EMACS	= emacs

ebin/%.beam: src/%.erl
	$(ERLC) -W -o ebin +debug_info $<

ebin/%.app: src/%.app.src
	sed -e 's/{modules, *\[/&$(MODULES)/' -e 's/,\]/]/' $< >$@

elisp/%.elc: elisp/%.el
	$(EMACS) -batch -L elisp -f batch-byte-compile $<

priv/%: c_src/%.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

all: $(APP) $(AOUT) $(BEAMS)

dialyze:
	dialyzer -Wunderspecs --src src

clean:
	rm -f $(AOUT) $(ELC) $(BEAMS) $(APP)
	rm -rf $(DSYM)
