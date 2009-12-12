.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -o ../ebin -W $<

MODS = all input_parser_tests db_tests main_tests

compile: ${MODS:%=%.beam}

all:
	compile
