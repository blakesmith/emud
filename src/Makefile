.SUFFIXES: .erl .beam .yrl

OUT = ../

.erl.beam:
	erlc -o ../ebin -W $< 

MODS = emud main input_parser db auth

compile: ${MODS:%=%.beam}

all:
	compile
