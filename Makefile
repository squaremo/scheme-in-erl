SRCS:=interp.erl grammar.erl compiler.erl
BEAMS:=$(patsubst %.erl,%.beam,$(SRCS))

.PHONY: all clean

all: $(BEAMS)

clean:
	rm $(BEAMS)
	rm grammar.erl

$(BEAMS): $(SRCS)
	erlc $(SRCS)

grammar.erl: neotoma/neotoma
	neotoma/neotoma grammar.peg

neotoma/neotoma:
	git clone git@github.com:seancribbs/neotoma.git
	(cd neotoma; make && make escript)
