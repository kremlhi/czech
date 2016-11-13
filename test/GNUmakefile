RM  ?= rm -f
SRC ?= $(wildcard *.erl)
BEAM = $(SRC:%.erl=%.beam)

test:
	ct_run -logdir ct_logs -include ../src -pa ../ebin $(SRC)

clean:
	$(RM) -r ct_logs/*
	$(RM) $(BEAM)
