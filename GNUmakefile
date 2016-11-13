CFLAGS += -ansi -pedantic -Wall -ggdb -O0
ERLC   ?= erlc
RM     ?= rm -f

SRC     = $(wildcard src/*.erl)
C_SRC   = $(wildcard c_src/*.c)
APP_SRC = $(wildcard src/*.app.src)

BEAM    = $(SRC:src/%.erl=ebin/%.beam)
PBEAM   = $(BEAM:%.beam=%.Pbeam)
MODULES = $(SRC:src/%.erl=%,)
APP     = $(APP_SRC:src/%.app.src=ebin/%.app)
AOUT    = $(C_SRC:c_src/%.c=priv/%)
DSYM    = $(C_SRC:c_src/%.c=priv/%.dSYM)
PLT     = $(notdir $(PWD)).plt
PLTAPPS+= $(shell sed -ne 's/{applications,//p' $(APP) | sed -e 's/[][},]//g')

# -pa ebin is needed for -behaviour
ebin/%.beam: src/%.erl
	$(ERLC) -MD -o ebin $<
	$(ERLC) -W -pa ebin -o ebin +debug_info $<

ebin/%.app: src/%.app.src
	sed -e 's/{modules, *\[/&$(MODULES)/' -e 's/,\]/]/' $< >$@

priv/%: c_src/%.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

all: $(BEAM) $(APP) $(AOUT)

$(PLT): $(APP)
	dialyzer --build_plt --output_plt $(PLT) --apps erts $(PLTAPPS)

build-plt: $(PLT)

dialyze: $(PLT)
	dialyzer -Wunderspecs --plt $(PLT) --src src

test: all
	$(MAKE) -C test

clean:
	$(MAKE) -C test clean
	$(RM) $(BEAM) $(PBEAM) $(APP) $(AOUT) $(PLT)
	$(RM) -r $(DSYM)

# p8 depends on czech, b/c -behaviour(czech).
src/p8.erl: ebin/czech.beam
#-include $(PBEAM)

.PHONY: test
