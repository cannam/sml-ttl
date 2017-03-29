
SCRIPTS	:= ../sml-buildscripts

BUILDER	:= ${SCRIPTS}/polybuild

all:	programs/load programs/convert example test/tests

programs/load:		programs/load.mlb d/load.deps 
	${BUILDER} $<

programs/convert:	programs/convert.mlb d/convert.deps 
	${BUILDER} $<

example:		example.mlb d/example.deps
	${BUILDER} $<

test/tests:		test/tests.mlb d/tests.deps
	${BUILDER} $<
	$@

MLBS	:= programs/load.mlb programs/convert.mlb example.mlb test/tests.mlb $(wildcard src/*.mlb)

d/load.deps:	programs/load.mlb $(MLBS)
	${SCRIPTS}/mlb-dependencies $< > $@

d/convert.deps:	programs/convert.mlb $(MLBS)
	${SCRIPTS}/mlb-dependencies $< > $@

d/example.deps:	example.mlb $(MLBS)
	${SCRIPTS}/mlb-dependencies $< > $@

d/tests.deps:	test/tests.mlb $(MLBS)
	${SCRIPTS}/mlb-dependencies $< > $@

clean:
	rm -f programs/load programs/convert example test/tests d/*

coverage:
	${SCRIPTS}/mlb-coverage test/tests.mlb

MLTON_ARGS	:= -runtime 'copy-generational-ratio 10.0' -runtime 'ram-slop 0.8'

release:
	mlton $(MLTON_ARGS) programs/load.mlb
	mlton $(MLTON_ARGS) programs/convert.mlb
	mlton $(MLTON_ARGS) example.mlb
	mlton $(MLTON_ARGS) test/tests.mlb
	test/tests

-include d/*.deps

