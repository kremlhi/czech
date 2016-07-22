CFLAGS += -ansi -pedantic -Wall -ggdb
ESRC	= $(wildcard *.erl)
ELISP	= $(wildcard *.el)
BEAMS	= $(ESRC:.erl=.beam)
ELC	= $(ELISP:.el=.elc)
AOUT	= p8adpt
ERLC	= erlc
EMACS	= emacs

%.beam: %.erl
	$(ERLC) -W +debug_info $<

%.elc: %.el
	$(EMACS) -batch -f batch-byte-compile $<

all: $(AOUT) $(BEAMS)

$(AOUT): $(AOUT).c

dialyze:
	dialyzer -Wunderspecs --src .

clean:
	rm -f $(AOUT) $(ELC) $(BEAMS)
	rm -rf *.dSYM
