CC = erlc

MODS = core.erl

compile:
	${CC} ${MODS}

all:
	compile
