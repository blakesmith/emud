.SUFFIXES: .erl .beam .yrl

OUT = ../

.erl.beam:
	erlc -o ../ebin -W $< 

MODS = core input_parser

compile: ${MODS:%=%.beam}

all:
	compile
