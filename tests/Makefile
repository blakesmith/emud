.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = input_parser_tests

compile: ${MODS:%=%.beam}

all:
	compile
