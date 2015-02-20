# Dirt simple Makefile for a dirt simple Erlang prohect.

ERLS = $(wildcard *.erl)
BEAMS = $(ERLS:.erl=.beam)

%.beam: %.erl %.hrl
	erlc $<

%.beam: %.erl
	erlc $<

all: $(BEAMS)

puzzle.beam: position.hrl

clean:
	rm *.beam
