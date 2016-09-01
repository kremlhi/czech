CFLAGS += -ansi -pedantic -Wall -ggdb

SRC	= $(wildcard src/*.erl)
BEAM	= $(SRC:src/%.erl=ebin/%.beam)
MODULES = $(SRC:src/%.erl=%,)
APP_SRC = $(wildcard src/*.app.src)
APP	= $(APP_SRC:src/%.app.src=ebin/%.app)
C_SRC	= $(wildcard c_src/*.c)
AOUT	= $(C_SRC:c_src/%.c=priv/%)
DSYM	= $(C_SRC:c_src/%.c=priv/%.dSYM)
ELISP	= $(wildcard elisp/*.el)
ELC	= $(ELISP:%.el=%.elc)

# load-path to distel and emms should be in users init-file/site-file
INITRC	= $(wildcard $(HOME)/.emacs $(HOME)/.emacs.d/init.el)
ERLC	= erlc
EMACS_COMMAND = emacs
EMACS	= $(EMACS_COMMAND) --batch -L elisp $(LOAD_INIT)

ifneq ($(INITRC),"")
LOAD_INIT = -l $(INITRC)
endif

ebin/%.beam: src/%.erl
	$(ERLC) -W -o ebin +debug_info $<

ebin/%.app: src/%.app.src
	sed -e 's/{modules, *\[/&$(MODULES)/' -e 's/,\]/]/' $< >$@

elisp/%.elc: elisp/%.el
	$(EMACS) -f batch-byte-compile $< 2>>elc.log

priv/%: c_src/%.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

all: $(BEAM) $(APP) $(AOUT) $(ELC)

dialyze:
	dialyzer -Wunderspecs --src src

clean:
	rm -f $(BEAMS) $(APP) $(AOUT) $(ELC) elc.log
	rm -rf $(DSYM)
