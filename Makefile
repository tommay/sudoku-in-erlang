# Dirt simple Makefile for a dirt simple Erlang prohect.

ERLS = $(wildcard *.erl)
BEAMS = $(ERLS:.erl=.beam)

%.beam: %.erl
	erlc $<

all: $(BEAMS)

puzzle.beam solver.beam: puzzle.hrl

clean:
	rm *.beam
