PARSER:=grammar.erl
SRCS:=$(PARSER) interp.erl compiler.erl primitives.erl cps.erl pp.erl
BEAMS:=$(patsubst %.erl,%.beam,$(SRCS))

.PHONY: all clean

all: $(BEAMS)

clean:
	rm -f $(BEAMS) $(PARSER)

%.beam: %.erl
	erlc $<

$(PARSER): neotoma/neotoma grammar.peg
	neotoma/neotoma grammar.peg

neotoma/neotoma:
	git clone git@github.com:seancribbs/neotoma.git
	(cd neotoma; make && make escript)
