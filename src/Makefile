.SUFFIXES: .erl .beam .yrl

OUT = ../

.erl.beam:
	erlc -o ../ebin -W $< 

MODS = emud core input_parser

compile: ${MODS:%=%.beam}

all:
	compile
