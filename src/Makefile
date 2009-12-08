.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = core input_parser

compile: ${MODS:%=%.beam}

all:
	compile
